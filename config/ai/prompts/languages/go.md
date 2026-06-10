<go_style>

Always use automated tools to manage dependencies in Go `BUILD` files. Follow this workflow:

- **Define imports in Go first**: Write your Go code and import the packages you need using the standard Google3 import path format (e.g., `import core "google3/prototypes/projects/cosmos/shared/shared"`).
- **Run `glaze` to auto-update `BUILD` files**: Run the `glaze` tool on your package directory. This tool will scan your Go files, detect the imports, and automatically update the `deps` in your `BUILD` file with the correct Bazel targets.

</go_style>
