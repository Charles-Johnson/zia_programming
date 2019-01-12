# izia
An interactive shell for Zia programming.

## When Building From Source

Assuming that the instructions found [here](../README.md) have been followed, the binary executable `izia` can be found in either the `target/debug` or `target/release` directory. Add this directory to your `PATH` environment variable.

## Usage

In the command line enter `izia`. This will print a short license notice followed by `>>>` as prompt. Type in a Zia expression and press enter. `izia` will execute these as commands.

```
>>> let ((not true) (-> false))

>>> (label_of((not true) ->)) ->
false
```
You can learn more about the Zia language [here](../zia/README.md).

## License

IZia is licensed under the General Public License (GPL), version 3 ([LICENSE](LICENSE) http://www.gnu.org/licenses/gpl-3.0.en.html).
