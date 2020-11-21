# ![bridge-sim](https://raw.githubusercontent.com/aornota/bridge/master/src/resources/tpoc-32x32.png) | bridge-sim (pre-_α_)

Work-in-progress on Bridge simulations (double-dummy analysis &c.).

Uses [Double Dummy Solver](http://privat.bahnhof.se/wb758135/bridge/index.html).

#### Development prerequisites

- [Microsoft .NET Core 3.1 SDK](https://dotnet.microsoft.com/download/dotnet-core/3.1/): I'm currently using 3.1.404

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
    - _paket install_
- Build targets:
    - Run the dev-console (debug): _fake build --target run-dev-console_ (or just _fake build_)
    - Help (lists key targets): _fake build --target help_ (or _fake build -t help_)
