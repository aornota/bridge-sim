# ![bridge-sim](https://raw.githubusercontent.com/aornota/bridge-sim/master/src/resources/tpoc-32x32.png) | bridge-sim (pre-_α_)

_Work-in-progress on Bridge simulations (double-dummy analysis &c.)..._

Uses [Double Dummy Solver (DDS)](http://privat.bahnhof.se/wb758135/bridge/index.html).

#### Development prerequisites

- [Microsoft .NET 8.0 SDK](https://dotnet.microsoft.com/download/dotnet/8.0): I'm currently using 8.0.410

##### Also recommended

- [Microsoft Visual Studio Code](https://code.visualstudio.com/download/) with the following extensions:
    - [Microsoft C#](https://marketplace.visualstudio.com/items?itemName=ms-vscode.csharp)
    - [Ionide-fsharp](https://marketplace.visualstudio.com/items?itemName=ionide.ionide-fsharp)
    - [EditorConfig for VS Code](https://marketplace.visualstudio.com/items?itemName=editorconfig.editorconfig)
    - [Rainbow Brackets](https://marketplace.visualstudio.com/items?itemName=2gua.rainbow-brackets)

#### Running

- Before first running:
    - Download _dds290-dll.zip_ from [Bridge DDS](http://privat.bahnhof.se/wb758135/bridge/dll.html), extract _dds.dll_ and _dds.lib_ from the relevant sub-folder (e.g. _dds-290-multi-x64-dll.zip_), and copy these to _/src/dds-interop_
    - _dotnet tool restore_
    - _dotnet paket restore_
- Build targets:
    - Run the dev-console (debug): _dotnet run run-dev-console_ (or just _dotnet run_)
    - Run the tests (Release): _dotnet run run-tests_
    - Help (lists key targets): _dotnet run help_
