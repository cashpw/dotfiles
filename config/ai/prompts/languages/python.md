<python_style>

*   **Build & Execution Rules**:
    *   Execute Python code and define dependencies exclusively through `BUILD` rules (`py_library` and/or `py_binary`). Avoid invoking the `python` or `python3` interpreters directly.
    *   Enforce strict type checking on all new build targets using `pytype` strict rules. Retain the existing configuration for all pre-existing targets. Use the following mappings:
        *   Use `pytype_strict_library` instead of `py_library`.
        *   Use `pytype_contrib_test` instead of `py_test`.
        *   Use `pytype_strict_binary` instead of `py_binary`.

*   **Style & Formatting**:
    *   Format all Python source code using `pyformat`.
    *   Represent multi-line text using multi-line strings (triple quotes) instead of concatenating strings with explicit newline characters (`\n`).

</python_style>
