<jj>

- Do NOT invoke `jj log -r ::@`. It outputs thousands of irrelevant lines of text. Use a more targetted command.
- **JJ Commit Description Updates**:
  - You **MUST** use `learning/gemini/agents/skills/create_cl/scripts/jj_safe_describe.sh -m '{commit_message}'` instead of `jj describe -m`.
  - `jj_safe_describe.sh` safely preserves any existing or prepopulated `JJCOPY` lines.
  - Always ensure changes to the commit description are **additive**; preserve all original details and append the new changes, as describing a commit overwrites the entire description.

</jj>
