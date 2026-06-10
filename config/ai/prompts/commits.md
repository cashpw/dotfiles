<commits>

- **Constraint Hierarchy**:
  - Priority 1: Maintain **TDD Integrity** (always commit code and tests together).
  - Priority 2: Use the exact commit message structure, including the **Startblock** code block and formatting constraints.
  - Priority 3: Always wrap commit messages in single-quotes when executing shell commands.

- **TDD Integrity**: Code and its corresponding tests must be committed together to ensure the build remains green.
- **Shell Safety**: When passing a commit description to shell commands, always wrap the message in single-quotes (e.g., `-m 'message'`) instead of double-quotes to prevent the shell from executing any backticks contained within the message.
- **Formatting**:
  - You MUST structure commit messages exactly per the template below.
  - The entire Startblock section (from `Startblock:` to the end of the block) MUST be wrapped in a triple-backtick code block in the final commit message.

<template>
````
[category]: [Short one-line description]

## What?

1. [First change]
2. [Second change]
3. [etc]

## Why?

[summary of purpose of these changes]

## Startblock

```
Startblock:
  # Ensure the CL is in a good state before sending it for review
  is passing Presubmit analysis
  is passing Lint analysis
  # Team review (diffbase okay)
  and then
  remember
  add reviewer guidance-backend+reviews
  has LGTM from everyone
  all comments are resolved
```

#AI

R=startblock
WANT_LGTM=ALL
MARKDOWN=true
STARTBLOCK_AUTOSUBMIT=yes
````

Where `category` is one of:

- `fix`: A bug fix
- `feat`: A new feature
- `docs`: Documentation
- `refactor`: A no-op change

</template>

</commits>
