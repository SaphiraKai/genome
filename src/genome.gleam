import gig/typed_ast
import glance

pub fn from_glance(
  glance_module: glance.Module,
  name: String,
  source: String,
) -> typed_ast.Context {
  typed_ast.infer_module(typed_ast.new_context(), glance_module, name, source)
}
// import gleam/bool
// import gleam/int
// import gleam/io
// import gleam/list
// import gleam/option.{None, Some}
// import gleam/pair
// import gleam/result
// import gleam/string

// import glance
// import shellout.{LetBeStderr}
// import simplifile

// import genome/module.{
//   type Function, type Import, type Module, Assert, Assignment, BinaryOperator,
//   Call, Expression, Fn, Function, Generate, Import, List, Module, Pipe, String,
//   Variable, VariablePattern,
// }

// import argv

// type Error {
//   Glance(inner: glance.Error)
//   Simplifile(inner: simplifile.FileError, msg: String)
//   MissingTarget(inner: Nil)
//   Compiler(inner: #(Int, String), msg: String)
// }

// pub fn main() {
//   case run() {
//     Error(e) ->
//       io.println_error(
//         "error: "
//         <> case e {
//           MissingTarget(_) -> "please provide a target module"
//           Simplifile(e, msg) -> msg <> " (" <> string.inspect(e) <> ")"
//           Compiler(e, msg) ->
//             msg
//             <> "; compiler returned code "
//             <> int.to_string(e.0)
//             <> "; stderr:\n"
//             <> e.1
//           Glance(e) -> "parsing failed (" <> string.inspect(e) <> ")"
//         },
//       )
//     _ -> Nil
//   }
// }

// fn run() -> Result(Nil, Error) {
//   let args = argv.load()

//   let keep_generated = fn(a: Function) {
//     list.any(a.attributes, fn(b) {
//       case b {
//         Generate(..) -> True
//         _ -> False
//       }
//     })
//   }

//   let import_module = fn(module: Module, import_: Import) {
//     let imports = case
//       module.imports |> list.find(fn(a) { a.module == import_.module })
//     {
//       Error(_) -> [import_, ..module.imports]
//       Ok(_) -> module.imports
//     }

//     Module(..module, imports:)
//   }

//   use target <- result.try(
//     list.first(args.arguments) |> result.map_error(MissingTarget),
//   )
//   let path = string.split("./gen/" <> target <> ".gleam", "/")
//   use source <- result.try(
//     simplifile.read(string.join(path, "/"))
//     |> result.map_error(Simplifile(
//       _,
//       "failed to read " <> string.join(path, "/"),
//     )),
//   )
//   use module <- result.try(
//     glance.module(source)
//     |> result.map(module.from_glance)
//     |> result.map_error(Glance),
//   )
//   let #(generate_functions, generate_modules) =
//     module.functions
//     |> list.filter(keep_generated)
//     |> list.map(fn(function) {
//       function.attributes
//       |> list.map(fn(attribute) {
//         let assert Generate(expression, module) = attribute
//         #(expression, module)
//       })
//     })
//     |> list.flatten
//     |> list.unzip
//   let module_dirs =
//     list.map(generate_modules, fn(a) {
//       "./src/generated_/"
//       <> string.split(a, "/")
//       |> list.reverse
//       |> list.drop(1)
//       |> list.reverse
//       |> string.join("/")
//     })
//     |> list.unique
//   let main_function =
//     Function(
//       attributes: [],
//       public: True,
//       name: "main",
//       parameters: [],
//       return: None,
//       body: [
//         Assignment(
//           kind: Assert,
//           pattern: VariablePattern("Ok(_)"),
//           value: Call(function: Variable(Some("list"), "try_each"), arguments: [
//             #("over", List(list.map(module_dirs, String), None)),
//             #(
//               "with",
//               Call(
//                 function: Variable(Some("simplifile"), "create_directory_all"),
//                 arguments: [#("", Variable(None, "_"))],
//               ),
//             ),
//           ]),
//         ),
//         ..list.zip(generate_functions, generate_modules)
//         |> list.map(fn(a) {
//           let #(function, module) = a
//           Assignment(
//             kind: Assert,
//             pattern: VariablePattern("Ok(_)"),
//             value: Call(
//               function: Variable(Some("simplifile"), "write"),
//               arguments: [
//                 #(
//                   "contents",
//                   Call(
//                     function: Variable(Some("module"), "to_string"),
//                     arguments: [#("", function)],
//                   ),
//                 ),
//                 #("to", String("./src/generated_/" <> module <> ".gleam")),
//               ],
//             ),
//           )
//         })
//       ],
//     )
//   let module =
//     Module(..module, functions: [main_function, ..module.functions])
//     |> import_module(Import(..module.new_import(), module: "gleam/io"))
//     |> import_module(Import(..module.new_import(), module: "simplifile"))
//   use _ <- result.try(
//     simplifile.delete("./build/genome")
//     |> result.try_recover(fn(e) {
//       case e {
//         simplifile.Enoent -> Ok(Nil)
//         e -> Error(e)
//       }
//     })
//     |> result.map_error(Simplifile(_, "failed to remove ./build/genome/")),
//   )
//   use _ <- result.try(
//     simplifile.delete("./src/generated_")
//     |> result.try_recover(fn(e) {
//       case e {
//         simplifile.Enoent -> Ok(Nil)
//         e -> Error(e)
//       }
//     })
//     |> result.map_error(Simplifile(_, "failed to remove ./src/generated_/")),
//   )
//   use _ <- result.try(
//     simplifile.create_directory_all("./build/genome")
//     |> result.map_error(Simplifile(_, "failed to create ./build/genome/")),
//   )
//   use entries <- result.try(
//     simplifile.read_directory("./")
//     |> result.map_error(Simplifile(_, "failed to read ./")),
//   )
//   use _ <- result.try(
//     entries
//     |> list.filter(fn(entry) {
//       ["build", "genome"] |> list.contains(entry) |> bool.negate
//     })
//     |> list.try_each(fn(entry) {
//       simplifile.copy(entry, "./build/genome/" <> entry)
//       |> result.map_error(pair.new(_, entry))
//     })
//     |> result.map_error(fn(error) {
//       Simplifile(error.0, "failed to copy " <> error.1 <> " to ./build/genome/")
//     }),
//   )

//   let path = ["build", "genome", "src", ..path]
//   let assert #(dir, [file]) = list.split(path, list.length(path) - 1)
//   use _ <- result.try(
//     simplifile.create_directory_all(string.join(dir, "/"))
//     |> result.map_error(Simplifile(
//       _,
//       "failed to create " <> string.join(dir, "/") <> "/",
//     )),
//   )

//   let path_string = string.join(dir, "/") <> "/" <> file
//   use _ <- result.try(
//     simplifile.write(path_string, module.to_string(module))
//     |> result.map_error(Simplifile(
//       _,
//       "failed to write module to " <> path_string,
//     )),
//   )
//   use _ <- result.try(
//     shellout.command(
//       run: "gleam",
//       with: [
//         "run",
//         "--target",
//         "javascript",
//         "--runtime",
//         "deno",
//         "-m",
//         "gen/" <> target,
//       ],
//       in: "./build/genome/",
//       opt: [LetBeStderr],
//     )
//     |> result.map_error(Compiler(_, "failed to generate module " <> target)),
//   )
//   use _ <- result.try(
//     simplifile.delete("./build/genome/gen/" <> target <> ".gleam")
//     |> result.map_error(Simplifile(_, "failed to delete ./build/genome/gen/")),
//   )
//   use _ <- result.try(
//     shellout.command(
//       run: "gleam",
//       with: ["format"],
//       in: "./build/genome/",
//       opt: [],
//     )
//     |> result.map_error(Compiler(_, "failed to run `gleam format`")),
//   )
//   use _ <- result.try(
//     simplifile.copy_directory(
//       "./build/genome/src/generated_",
//       "./src/generated_",
//     )
//     |> result.map_error(Simplifile(_, "failed to copy generated code")),
//   )

//   Ok(Nil)
// }
