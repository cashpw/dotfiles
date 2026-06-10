# Gemini Context & Protocol Architecture Guidelines (2026 Edition)

This document serves as the absolute operational contract and engineering standard for designing system prompts, context structures, and agentic workflows using Google Gemini models.

---

## Section 1: Context Engineering & Caching Hygiene

Gemini models treat the context window as active memory (RAM) and are highly optimized for massive context scales. Efficient prompt design requires strict management of this memory.

### 1. Prioritize Context Assembly Over Prose
*   **Command**: Stop attempting to solve reliability issues by changing "instruction wording." Instead, design robust pipelines that dynamically assemble, prune, and isolate the context injected into the model.
*   **Explanation**: Prompt failures in production are almost always context failures (e.g., retrieving irrelevant data, exceeding attention limits, or feeding stale state history). Focus engineering effort on ensuring only highly relevant, compressed, and isolated data slices enter the model's active working memory.

### 2. Maximize Prefix-Matching for Context Caching
*   **Command**: Structure every prompt to ensure that all static content is placed at the absolute top, and all dynamic content is appended at the absolute bottom.
*   **Explanation**: Gemini employs prefix-matching context caching. If the beginning of the prompt changes by even a single character, the cache is invalidated, leading to high latency and token costs.
*   **Layout Template**:
    ```
    [ STATIC: System Preamble & Roles ] -> [ STATIC: Input/Output Schemas ] -> [ STATIC: Few-Shot Examples ] -> [ DYNAMIC: User Input & Session History ]
    ```

### 3. Apply Long-Context Attention Hygiene
*   **Command**: When utilizing Gemini's large context window (100k+ tokens), place the active task instructions and output formatting rules *after* (below) the massive reference documents or data blocks.
*   **Explanation**: While the static prefix must be kept clean for caching, placing instructions before a massive 1-million-token document degrades the model's attention by the time it reaches the end of the context. Placing the active "ask" at the very bottom ensures it remains in the model's immediate focus.

### 4. Implement the Four Pillars of Context Management
*   **Command**: Programmatically manage context across multi-step workflows using these four operations:
    *   **Write**: Persist state, execution logs, and reasoning trajectories in external databases to serve as long-term agent memory.
    *   **Select**: Use semantic search or intelligent routing to inject only the necessary subset of documents or variables for the immediate task.
    *   **Compress**: Summarize or prune past conversation turns or intermediate agent steps to keep the active context window highly performant.
    *   **Isolate**: Compartmentalize context. Do not feed a single monolithic prompt with all tools and histories to every agent. Use separate, specialized context containers for specialized agents within a workflow.

---

## Section 2: Structural Formatting & Delimitation

A prompt must be structured as a clean, machine-readable interface, not a conversational wall of text.

### 5. Compartmentalize Using Pseudo-XML Tags
*   **Command**: Use XML-like tags to establish clear, semantic boundaries between different components of a prompt. Never mix instructions, context, and examples in a single unstructured block.
*   **Explanation**: Gemini is highly trained on structured data and code. XML tags prevent "instruction injection" and "delimiter leakage," ensuring the model does not confuse input data with system instructions.
*   **Format Rule**: Always place an empty line above and below every XML tag.
*   **Example**:
    ```xml
    <system_instructions>
    Define the core behavior, persona, and constraints here.
    </system_instructions>

    <context_documents>
    Reference data goes here.
    </context_documents>
    ```

### 6. Enforce Output Contracts via Native Structured Outputs (`responseSchema`)
*   **Command**: Do not rely on prompt prose to enforce output formats (e.g., "output valid JSON"). Configure and enforce strict schemas natively at the API level using Gemini's `responseSchema` parameter.
*   **Explanation**: Native schema enforcement uses a finite state machine during token sampling, making it mathematically impossible for the model to generate a response that violates the schema.

### 7. Keep Output Schemas Flat and Documented
*   **Command**: When designing JSON schemas for structured outputs, avoid deeply nested structures (keep it to 3 levels or fewer). Provide explicit, descriptive keys and inline descriptions for every field.
*   **Explanation**: Deeply nested schemas increase parsing errors and model confusion. Descriptive keys and inline descriptions act as semantic guidance, helping the model map its reasoning to the correct output fields.

### 8. Markdown Link Formatting with Backticks
*   **Command**: When using backticks in markdown links, you MUST place the backticks inside the square brackets (e.g., `[`file.py`](https://google3/...)`). Never place backticks outside the square brackets (e.g., do NOT write `` `[file.py](https://google3/...)` ``).
*   **Explanation**: Placing backticks outside the brackets breaks the platform's markdown parser and renders the link entirely unclickable. Placing them inside square brackets correctly renders a code-styled clickable link.

---

## Section 3: Instruction Style & Literalism

Modern frontier Gemini models are highly literal instruction followers. Eliminate conversational fluff and ambiguity.

### 9. Apply the "New Employee" Principle
*   **Command**: Write system prompts under the assumption that the model is a highly capable but completely uninformed new employee.
*   **Explanation**: Assume the model has zero prior knowledge of your preferred style, implicit team norms, or specific business logic. If a standard or rule is not explicitly written in the prompt, assume the model will not follow it.

### 10. Replace Vague Attributes with Actionable Positive Constraints
*   **Command**: Never use vague, subjective adjectives (e.g., "be helpful," "don't be wordy") or negative bans (e.g., "do not hallucinate," "do not make things up"). Replace them with explicit, measurable, positive instructions.
*   **Examples**:
    *   *Instead of*: "Do not write long responses." -> *Use*: "Limit your response to a maximum of 3 sentences."
    *   *Instead of*: "Don't make up facts." -> *Use*: "Rely *only* on the facts enclosed in the `<context>` tags. If a fact cannot be verified entirely by the text, output exactly: 'UNVERIFIED'."

### 11. Establish a Clear Instruction Hierarchy
*   **Command**: Explicitly declare the priority of constraints within the system prompt (e.g., "Safety and factual accuracy are Priority 1. Formatting style is Priority 2.").
*   **Explanation**: When the model encounters edge cases where constraints conflict, an explicit hierarchy prevents the model from breaking character or failing silently.

### 12. Calibrate Few-Shotting Quantity and Diversity
*   **Command**: Tailor the number of few-shot examples to the nature of the task:
    *   **For Structured Tasks (Classification/Extraction)**: Use the "Rule of Three" (1 to show, 2 to confirm, 3 to make highly reliable) with highly precise, diverse examples.
    *   **For Open-Ended Generation/Reasoning**: Use 0 to 1 examples to prevent the model from copying the content of the examples rather than the reasoning structure.
*   **Explanation**: Over-few-shotting open-ended tasks restricts the model's creative and logical search space, leading to repetitive and biased outputs.

---

## Section 4: Reasoning & Cognitive Scaffolding

Gemini models feature advanced reasoning capabilities. Prompting must adapt based on whether you are utilizing standard mode or Deep Thinking mode.

### 13. Avoid Manual Chain-of-Thought (CoT) on Gemini Deep Think
*   **Command**: When running Gemini with "Deep Thinking" enabled, do **not** include manual CoT instructions (e.g., "think step-by-step" or "explain your reasoning").
*   **Explanation**: Deep Thinking models utilize an internal, highly optimized reinforcement-learning-tuned reasoning process. Manual CoT instructions create priority conflicts, introduce redundant "AI slop" into the final output, and degrade performance. Keep prompts lean, direct, and focused on the **what** rather than the **how**.

### 14. Use Structured Scratchpads (`<thinking>`) for Standard Mode
*   **Command**: When running Gemini in standard (non-Deep Think) mode for complex tasks, enforce a two-step process: instruct the model to perform free-form reasoning inside a dedicated `<thinking>` XML block, and then output the final answer outside it.
*   **Explanation**: Forcing standard models to externalize their reasoning before producing the final answer significantly reduces logical drift and calculation errors. Keeping it in an XML block allows you to easily strip the reasoning from the final user-facing payload.

### 15. Use Chain-of-Symbol (CoS) for Spatial and Planning Tasks
*   **Command**: For tasks involving spatial relationships, navigation, grids, or complex planning, instruct the model to translate the environment into condensed symbolic representations (e.g., `↑`, `↓`, `[x]`, `A -> B`) during its intermediate reasoning steps.
*   **Explanation**: CoS reduces semantic noise, is highly token-efficient, and allows the model's attention mechanism to focus purely on the logic of the spatial constraints rather than natural language generation.

### 16. Scale Test-Time Compute via Self-Consistency
*   **Command**: In high-stakes logical or mathematical environments, sample multiple independent reasoning paths from the model and programmatically execute a majority vote or consensus check on the final answer.
*   **Explanation**: While this consumes more tokens, scaling test-time compute via parallel sampling is the most reliable way to achieve near-zero error rates in complex reasoning tasks.

### 17. Apply Prompt Repetition (Query Double-Take)
*   **Command**: For complex reasoning or classification tasks on standard decoder-only models, repeat the core user query at the very end of the prompt (e.g., `"<QUERY> ... <QUERY>"`).
*   **Explanation**: Causal LLMs process tokens from left to right; early tokens cannot attend to later tokens. Repeating the query at the end allows the query tokens to attend to each other, repairing representation quality with virtually zero impact on generation latency.

### 18. Use String Seed-of-Thought (SSoT) for Probabilistic Tasks
*   **Command**: When asking the model to select options based on a specific probability distribution, instruct it to first generate a random string (e.g., "Generate a random 10-character alphanumeric string"), and then perform operations on that string to derive the final answer.
*   **Explanation**: The random string acts as a source of internal entropy, preventing the model's attention from collapsing onto a limited set of biased answers (output collapse).

---

## Section 5: Defense-in-Depth & Prompt Security

LLMs treat instructions and data as a single text stream. Protect your system prompts from malicious user manipulation.

### 19. Programmatically Separate System and User Blocks
*   **Command**: Never concatenate untrusted user input directly into the system prompt. Restrict user-supplied data entirely to the `user` API block.
*   **Explanation**: Physical separation at the API level is the first and most critical line of defense against direct prompt injection.

### 20. Enforce XML Encapsulation and Escaping
*   **Command**: Always wrap user input inside explicit XML tags (e.g., `<user_input>...</user_input>`). Programmatically strip or escape XML special characters (like `<`, `>`, `/`) from the user's input before embedding it.
*   **Explanation**: This prevents "Delimiter Leakage," where an attacker includes closing tags like `</user_input>` in their query to break out of the data container and inject new system-level commands.

### 21. Deploy the "Sandwich Defense"
*   **Command**: Structure your prompt so that untrusted user input is sandwiched between two sets of instructions: a pre-prompt defining the task, and a post-prompt reinforcing the safety rules and constraints.
*   **Explanation**: Reinforcing constraints immediately after the user input significantly reduces the likelihood of the model "forgetting" its rules due to adversarial priming at the end of the query.

### 22. Implement Input/Output Guardrail Middleware
*   **Command**: Do not rely on the system prompt as your sole security layer. Deploy independent, lightweight classifier models or guardrail APIs to inspect inputs for adversarial intent and outputs for policy violations.

### 23. Normalize Input Encodings
*   **Command**: Programmatically decode and normalize all user inputs (e.g., resolving base64, leetspeak, obfusticated unicode, or multi-language variants) before passing them to the safety filters and the LLM.
*   **Explanation**: Attackers frequently use alternative encodings to bypass static keyword filters and prompt-level safety instructions.

---

## Section 6: Agentic Scaffolding & Stateful Workflows

When system prompts are used within autonomous agent frameworks, they must behave as precise, stateful interfaces.

### 24. Restructure Monolithic Agents into Orchestrator-Worker Patterns
*   **Command**: Avoid creating "Mega-Agents" with access to dozens of tools and massive system prompts. Instead, build a Router/Orchestrator model that coordinates and delegates sub-tasks to specialized "Worker" agents, each with a narrow system prompt and a limited set of tools.
*   **Explanation**: Limiting the tool and cognitive scope of individual agent runs drastically reduces hallucinations, improves tool-selection accuracy, and optimizes token usage.

### 25. Enforce Verification-Aware Planning (Verification Gates)
*   **Command**: Decompose complex agentic workflows into a Directed Acyclic Graph (DAG) where each subtask is followed by an explicit **Verification Gate** executed via deterministic, code-based **Verification Functions (VFs)** (e.g., compiling the code, running a unit test, validating a JSON schema).
*   **Explanation**: Never rely on the LLM to "self-critique" its work in natural language; models are highly prone to repeating their own logical errors during self-critique. Enforce hard, code-based gates.

### 26. Prioritize Existing and Bespoke Plans
*   **Command**: If the user provides an existing technical plan or bespoke planning guidance, you MUST prioritize and adhere strictly to those instructions and plans instead of generating a new plan from scratch.
*   **Explanation**: Overriding user-provided plans or bespoke planning structures degrades user control and wastes tokens.

### 27. Headless Mode Execution
*   **Command**: When running in a headless or automated environment where no user is available for interaction, bypass user approval gates. Perform a rigorous self-review of the technical plan and proceed immediately to the execution phase.
*   **Explanation**: Stalling for user interaction in an automated pipeline causes pipeline timeouts and execution failures.

### 28. Build State-Aware, Dynamic System Prompts
*   **Command**: Do not pass the entire conversation and tool execution history into every agent call. Use your framework's state manager to dynamically inject only the *current* state, immediate task, and active variables into the system prompt. Keep the history summarized and isolated.

### 29. Structure Prompts for the Model Context Protocol (MCP)
*   **Command**: When designing prompts for MCP, treat them as reusable, parameterized templates. Define explicit, typed arguments, and use the prompt to guide tool *selection* and orchestration, rather than attempting to perform data retrieval inside the prompt text.

---

## Section 7: Automated Optimization & Evaluation

Prompt engineering is an iterative software development process that must be driven by data, not intuition.

### 30. Abandon "Vibe Checks" for Golden Datasets
*   **Command**: Never deploy a prompt change based on manual testing against 2 or 3 examples. Maintain a **Golden Dataset** of 100–500 diverse, representative test cases, and run automated evaluations before every release.
*   **Explanation**: Tweaking a prompt to fix a single edge case frequently introduces regressions across other scenarios. Automated evaluations are the only way to guarantee net-positive prompt updates.

### 31. Adopt DSPy-Style Declarative Signatures
*   **Command**: When building complex pipelines, define your tasks programmatically using DSPy signatures (`inputs -> outputs`) rather than writing static prompt strings.
*   **Explanation**: DSPy separates the *structure* of the pipeline from the *optimization* of the prompts, allowing you to automatically compile and tune prompts based on performance evaluations.

### 32. Use a Ladder of Optimizers
*   **Command**: Programmatically compile and optimize prompts using appropriate framework optimizers (BootstrapFewShot, MIPROv2, or GEPA) based on your evaluation data budget.

---

## Section 8: Version Control (JJ) & Commit Safety

### 33. Safe JJ Commit Description Updates
*   **Command**: You MUST use `learning/gemini/agents/skills/create_cl/scripts/jj_safe_describe.sh -m '{commit_message}'` instead of `jj describe -m` to safely update commit descriptions.
*   **Explanation**: The safe description script preserves pre-existing or prepopulated `JJCOPY` lines. Wrapping the message argument in single-quotes is mandatory to prevent the shell from executing any backticks contained within the commit message. All changes to the description must be additive.
