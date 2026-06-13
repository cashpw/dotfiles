<jj>

- Do NOT invoke `jj log -r ::@`. It outputs thousands of irrelevant lines of text. Use a more targetted command.
- **JJ Commit Description Updates**:
    - You **MUST NOT** use `jj describe -m` with a raw string if your message contains special characters like apostrophes (`'`) or backticks (`` ` ``), as this is highly prone to shell escaping failures.
    - Instead, you **MUST** write your complete commit message to a temporary file (e.g., in your scratch directory) and apply it using `jj describe --stdin < file_path` (or `cat file_path | jj describe --stdin`).
    - **Preserving JJCOPY Lines (CRITICAL)**: Describing a commit overwrites the entire description. You MUST safely preserve any existing `JJCOPY:` lines (which track file moves/copies) that are already present in the commit description:
        1. Before describing, run `jj log --no-graph -T description -r @` to read the current description of the target commit.
        2. Extract all lines starting with `JJCOPY:` (e.g., `JJCOPY: //depot/google3/foo -> //depot/google3/bar`).
        3. Construct your new commit message, and append the extracted `JJCOPY:` lines to the very bottom of the message (after all other tags like `BUG=`, `FIXED=`, `TAG=agy`).
        4. Write this combined message to a temporary file.
        5. Apply it using `jj describe --stdin < file_path`.
- When moving or copying files or extracting something into a new file in a `jj` (Jujutsu) workspace on Piper, you **must** mark them as moved/branched to preserve file history in Critique and Piper.
    - Use the dedicated `jj piper` commands to perform the operation. This automatically records the metadata.
        -   Move/rename: `jj piper rename <source_file> <target_file>`
        -   Copy/branch: `jj piper copy <source_file> <target_file>`
    - Retroactive Tracking (After the move/copy has already happened): If you already moved/copied files (e.g. via IDE or `mv`/`cp` commands), you can mark them retroactively.
        -   Mark as moved: `jj piper rename --after <source_file> <target_file>`
        -   Mark as copied: `jj piper copy --after <source_file> <target_file>`
        -   For a specific revision (e.g. parent commit `@-`): Add the `-r` flag: `jj piper rename --after -r @- <source_file> <target_file>`
    - How it works under the hood:
        - These commands append `JJCOPY:` annotations to your commit description: `JJCOPY: //depot/google3/path/to/source -> //depot/google3/path/to/target`
        - Renames are represented as a `JJCOPY` annotation plus the deletion of the source file in the same commit.

</jj>
