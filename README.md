# ![bridge-sim](https://raw.githubusercontent.com/aornota/bridge-sim/master/src/resources/tpoc-32x32.png) | bridge-sim (pre-_α_)

_Work-in-progress on Bridge simulations (double-dummy analysis &c.)..._

Uses [Double Dummy Solver](http://privat.bahnhof.se/wb758135/bridge/index.html).

#### Development prerequisites

- [Microsoft .NET 5.0 SDK](https://dotnet.microsoft.com/download/dotnet/5.0): I'm currently using 5.0.400

##### Also recommended

- [Microsoft Visual Studio Code](https://code.visualstudio.com/download/) with the following extensions:
    - [Microsoft C#](https://marketplace.visualstudio.com/items?itemName=ms-vscode.csharp)
    - [Ionide-fsharp](https://marketplace.visualstudio.com/items?itemName=ionide.ionide-fsharp)
    - [EditorConfig for VS Code](https://marketplace.visualstudio.com/items?itemName=editorconfig.editorconfig)
    - [Rainbow Brackets](https://marketplace.visualstudio.com/items?itemName=2gua.rainbow-brackets)
- ([Microsoft .NET Framework 4.7.2 Developer Pack](https://dotnet.microsoft.com/download/dotnet-framework/net472/): this appeared to resolve problems with Intellisense in
_[build.fsx](https://github.com/aornota/gibet/blob/master/build.fsx)_)

#### Running

- Before first running:
    - _dotnet tool restore_
    - _dotnet paket install_
- Build targets:
    - Run the dev-console (debug): _dotnet fake build --t run-dev-console_ (or just _dotnet fake build_)
    - Run the tests (Release): _fake build --t run-tests_
    - Help (lists key targets): _dotnet fake build -t help_
