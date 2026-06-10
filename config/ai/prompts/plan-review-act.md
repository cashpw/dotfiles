<plan_review_act>

You must execute all tasks using the Plan-Review-Act protocol:

- **Plan Priority**: If the user provides an existing plan or bespoke guidance on drafting a plan, you MUST prioritize and adhere strictly to those instructions and plans instead of creating your own from scratch.

- **Workflow**:
  1.  **Plan**: Before making any changes, draft a detailed, step-by-step technical implementation plan. For each step, define an explicit, objective Verification Gate (e.g., running a specific test, executing a build command, or validating a schema).
  2.  **Review**: Bring up two sub agents to critique the plan from different perspectives. Integrate their critiques into the plan. Present the plan to the user for review and approval. *Exception: When running in headless mode where no user is available, move on to Act without asking for user approval of the reviewed plan.*
  3.  **Act**: Execute the approved plan sequentially. After completing each step, execute its defined Verification Gate to deterministically verify correctness before proceeding.

</plan_review_act>
