# OAS Generator

Generate [Gleam](https://gleam.run/) clients from [Open API Specs](https://swagger.io/specification/).

[![Package Version](https://img.shields.io/hexpm/v/oas_generator)](https://hex.pm/packages/oas_generator)
[![Hex Docs](https://img.shields.io/badge/hex-docs-ffaff3)](https://hexdocs.pm/oas_generator/)


## ✨ Features

- 🦄 Generates Gleam HTTP client code from OpenAPI 3.0+ specs
- 🔒 Produces typesafe function signatures and records
- 🧩 Compatible with `gleam/httpc` or `gleam/fetch` for use on front end or backend.
- 💾 Parses JSON responses into Gleam types


Doesn't support all uses of datatype composition for example `AnyOf` and `OneOf`.

## 🛠️ Usage

Install as a dev dependency.

```sh
gleam add --dev oas_generator@1
```

Fetch the JSON version of your API specification.

An example for a petstore is available [here](https://petstore3.swagger.io/api/v3/openapi.json)

Create a module that will call the `build` function.

Adding the module to the test dir means it is not added to the published library.
Support for a dev directory is coming later https://discord.com/channels/768594524158427167/1047099923897794590/threads/1365928769956610090

```gleam
// test/petstore/dev
import gleam/io
import oas/generator
import snag

pub fn main() {
  case generator.build("./priv/petstore.openapi.json", ".", "petstore") {
    Ok(_) -> Nil
    Error(reason) -> io.print(snag.pretty_print(reason))
  }
}
```

Run the generator.

```
gleam run -m petstore/dev
```

Add functions to build your base request.

*OAS Generator does not support authentication yet, you will need to add this yourself.*

To handle this the generated code leaves the following as hooks to implement in the `<project>.gleam` file.
The required functions are `handle_errors` and `base_request`. To see an example of these check out the netlify implementation [here](https://github.com/midas-framework/midas_sdk/blob/main/sdks/netlify/src/netlify.gleam)

Code above the `// GENERATED -------------` line will be left alone.

*OAS Generator uses Midas to compose request and response functions.*

If you do not want to use Midas you can delete the top project file and use the functions in `operations.gleam` directly.
Running with midas requires you to use a runner. Runners are available for node and the browser. 
It is also faily easy to write your own runner, if you wish to only handle the fetch effect

For further help reach out in the [Gleam Discord](https://discord.com/channels/768594524158427167)

## 📚 Documentation
Further documentation can be found at <https://hexdocs.pm/oas_generator>.

## 🧪 Development

```sh
gleam test  # Run the tests
```

## 🧑‍💻 Credit
Created for [Spotless](https://github.com/CrowdHailer/gleam_spotless), a code first integration platform.