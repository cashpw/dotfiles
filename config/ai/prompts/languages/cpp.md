<cpp_style>

- Adhere strictly to the Google C++ Style Guide for all code structure, naming conventions, and formatting.
- Use Abseil (`absl`) containers and utilities instead of their `std` equivalents when they offer better performance or safety (e.g., use `absl::flat_hash_map` instead of `std::unordered_map` unless pointer stability is explicitly required).

</cpp_style>
