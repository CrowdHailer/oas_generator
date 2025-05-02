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

## 📚 Documentation
Further documentation can be found at <https://hexdocs.pm/oas_generator>.

## 🧪 Development

```sh
gleam test  # Run the tests
```

## 🧑‍💻 Credit
Created for [Spotless](https://spotless.run), a code first integration platform.