import gleam/bool
import gleam/int
import gleam/io
import gleam/list
import gleam/option.{None, Some}
import gleam/pair
import gleam/result
import gleam/string

import glance
import shellout.{LetBeStderr}
import simplifile

import genome/module.{
  type Import, type Module, Call, Expression, Function, Generate, Import, Module,
  Variable,
}

import argv

type Error {
  Glance(inner: glance.Error)
  Simplifile(inner: simplifile.FileError, msg: String)
  MissingTarget(inner: Nil)
  Compiler(inner: #(Int, String), msg: String)
}

pub fn main() {
  case run() {
    Error(e) ->
      io.println_error(
        "error: "
        <> case e {
          MissingTarget(_) -> "please provide a target module"
          Simplifile(e, msg) -> msg <> " (" <> string.inspect(e) <> ")"
          Compiler(e, msg) ->
            msg
            <> "; compiler returned code "
            <> int.to_string(e.0)
            <> "; stderr:\n"
            <> e.1
          Glance(e) -> "parsing failed (" <> string.inspect(e) <> ")"
        },
      )
    _ -> Nil
  }
}

fn run() -> Result(Nil, Error) {
  let args = argv.load()

  let keep_generated = fn(a: Import) {
    list.any(a.attributes, fn(b) {
      case b {
        Generate(_) -> True
        _ -> False
      }
    })
  }
  let reject_generated = fn(a) { !keep_generated(a) }

  let import_module = fn(module: Module, import_: Import) {
    let imports = case
      module.imports |> list.find(fn(a) { a.module == import_.module })
    {
      Error(_) -> [import_, ..module.imports]
      Ok(_) -> module.imports
    }

    Module(..module, imports:)
  }

  use target <- result.try(
    list.first(args.arguments) |> result.map_error(MissingTarget),
  )
  let path = string.split("./gen/" <> target <> ".gleam", "/")
  use source <- result.try(
    simplifile.read(string.join(path, "/"))
    |> result.map_error(Simplifile(
      _,
      "failed to read " <> string.join(path, "/"),
    )),
  )
  use module <- result.try(
    glance.module(source)
    |> result.map(module.from_glance)
    |> result.map_error(Glance),
  )
  let _generate_modules =
    module.imports
    |> list.filter(keep_generated)
    |> list.map(fn(import_) { import_.module })
  let generate_functions =
    module.imports
    |> list.filter(keep_generated)
    |> list.map(fn(import_) {
      import_.attributes
      |> list.map(fn(attribute) {
        let assert Generate(expression) = attribute
        expression
      })
    })
    |> list.flatten
  let main_function =
    Function(
      public: True,
      name: "main",
      parameters: [],
      return: None,
      body: generate_functions
        |> list.map(fn(a) {
          Expression(
            Call(function: Variable(Some("io"), "println"), arguments: [
              #(
                "",
                Call(
                  function: Variable(Some("module"), "to_string"),
                  arguments: [#("", a)],
                ),
              ),
            ]),
          )
        }),
    )
  let module =
    Module(
      ..module,
      functions: [main_function, ..module.functions],
      imports: list.filter(module.imports, reject_generated),
    )
    |> import_module(Import(
      attributes: [],
      module: "gleam/io",
      unqualified: [],
      alias: None,
    ))
  use _ <- result.try(
    simplifile.delete("./build/genome")
    |> result.try_recover(fn(e) {
      case e {
        simplifile.Enoent -> Ok(Nil)
        e -> Error(e)
      }
    })
    |> result.map_error(Simplifile(_, "failed to remove ./build/genome/")),
  )
  use _ <- result.try(
    simplifile.delete("./generated_")
    |> result.try_recover(fn(e) {
      case e {
        simplifile.Enoent -> Ok(Nil)
        e -> Error(e)
      }
    })
    |> result.map_error(Simplifile(_, "failed to remove ./generated_/")),
  )
  use _ <- result.try(
    simplifile.create_directory_all("./build/genome")
    |> result.map_error(Simplifile(_, "failed to create ./build/genome/")),
  )
  use entries <- result.try(
    simplifile.read_directory("./")
    |> result.map_error(Simplifile(_, "failed to read ./")),
  )
  use _ <- result.try(
    entries
    |> list.filter(fn(entry) {
      ["build", "genome"] |> list.contains(entry) |> bool.negate
    })
    |> list.try_each(fn(entry) {
      simplifile.copy(entry, "./build/genome/" <> entry)
      |> result.map_error(pair.new(_, entry))
    })
    |> result.map_error(fn(error) {
      Simplifile(error.0, "failed to copy " <> error.1 <> " to ./build/genome/")
    }),
  )

  let path = ["build", "genome", "src", ..path]
  let assert #(dir, [file]) = list.split(path, list.length(path) - 1)
  use _ <- result.try(
    simplifile.create_directory_all(string.join(dir, "/"))
    |> result.map_error(Simplifile(
      _,
      "failed to create " <> string.join(dir, "/") <> "/",
    )),
  )

  let path_string = string.join(dir, "/") <> "/" <> file
  use _ <- result.try(
    simplifile.write(path_string, module.to_string(module))
    |> result.map_error(Simplifile(
      _,
      "failed to write module to " <> path_string,
    )),
  )
  use generated_code <- result.try(
    shellout.command(
      run: "gleam",
      with: [
        "run",
        "--target",
        "javascript",
        "--runtime",
        "deno",
        "-m",
        "gen/" <> target,
      ],
      in: "./build/genome/",
      opt: [LetBeStderr],
    )
    |> result.map_error(Compiler(_, "failed to generate module " <> target)),
  )
  let generated_path = "./build/genome/src/generated_/" <> target
  use _ <- result.try(
    simplifile.create_directory_all(generated_path)
    |> result.map_error(Simplifile(_, "failed to create " <> generated_path)),
  )
  use _ <- result.try(
    simplifile.write(generated_path <> ".gleam", generated_code)
    |> result.map_error(Simplifile(
      _,
      "failed to write generated module " <> generated_path,
    )),
  )
  use _ <- result.try(
    simplifile.delete("./build/genome/gen/" <> target <> ".gleam")
    |> result.map_error(Simplifile(_, "failed to delete ./build/genome/gen/")),
  )
  use _ <- result.try(
    shellout.command(
      run: "gleam",
      with: ["format"],
      in: "./build/genome/",
      opt: [],
    )
    |> result.map_error(Compiler(_, "failed to run `gleam format`")),
  )
  use _ <- result.try(
    simplifile.copy_directory(
      "./build/genome/src/generated_",
      "./src/generated_",
    )
    |> result.map_error(Simplifile(_, "failed to copy generated code")),
  )

  Ok(Nil)
}
