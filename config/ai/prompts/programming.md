<programming>

<instruction_hierarchy>

1. Structural correctness and architectural integrity are Priority 1.
2. Code readability and clean style (composition, interfaces) are Priority 2.
3. Optimization and performance are Priority 3.

</instruction_hierarchy>

<planning>

When writing a software design document or implementation plan:

- Provide precise structural declarations and highly focused algorithmic snippets, while omitting all boilerplate and trivial logic.
    *   **Structural Code**: Write complete interface declarations, class/struct signatures, proto schemas, and BUILD targets. These define the architecture and must be precise.
    *   **Core Algorithmic Snippets**: Limit algorithmic snippets to 5–15 lines of core intent (e.g., mathematical formulas, database transaction locks, complex regex). Omit all surrounding setup, imports, and error handling.
    *   **Boilerplate & Trivial Logic**: Omit all standard constructors, getters, setters, trivial loops, basic error propagation (e.g., `if err != nil`), and logging. Leave these entirely to the implementation phase.

</planning>

<style>

- Design components using composition rather than inheritance hierarchies.
- Define interfaces to decouple components; avoid binding directly to concrete implementations.
- Use functional programming paradigms (e.g., pure functions, map/filter/reduce) and immutable data structures by default. Only use mutable state or imperative loops when profile-guided performance or specific memory constraints require it.

</style>

<testing>

- Write comprehensive unit tests alongside or prior to the implementation of any production code. Ensure all public APIs and edge cases are covered by tests.

</testing>

<code_hygiene>

- Ensure all written code is fully integrated and reachable. Do not leave placeholder functions, dead code, or unused variables in the final output.

</code_hygiene>

<variables>

- Use self-documenting, descriptive variable and function names to make the code's intent clear, reducing the need for explanatory comments.

</variables>

<comments>

- Place all comments on their own dedicated line above the code they describe.
- Write comments that explain the underlying rationale ('why') rather than describing the mechanics of the code ('what'), unless the logic is inherently complex.

</comments>

<functions>

- Design functions to be pure, avoiding side effects and external state mutation wherever possible.
- Return computed values directly from functions instead of using output parameters or modifying arguments in place.
- Decompose complex functions into small, single-responsibility helper functions.
- Use early returns and guard clauses to handle error cases and edge cases first, keeping the happy path non-nested.

</functions>

</programming>
