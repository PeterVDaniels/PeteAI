# User Direction Sheet for PeteAI

This guide is for running the `PeteAI_Groks_Alt_Rlm_LA_SUB_Cltr_CmGt_Hb_165r7_5_26_25.lisp` program, a Lisp-based AI that simulates quantum effects in memory flow, visualizes conversations with neural diagrams, and uses an internal treaty for balanced reasoning. The program is designed to be interactive, allowing you to input conversations and visualize the AI’s thought process.

## Audience
- **Beginners**: Users comfortable with a terminal but new to Lisp interpreters like SBCL.
- **Knowledgeable Users**: Users familiar with Lisp, SBCL, and terminal commands, looking for a quick setup and advanced usage tips.

## Section 1: Quick Setup Guide

### For Beginners: Setting Up the Environment
If you’re new to Lisp and SBCL, follow these steps to get started on a Linux system (e.g., Fedora, Ubuntu). This assumes you have basic terminal skills but no prior Lisp experience.

1. **Install SBCL (the Lisp Interpreter)**:
   - Open your terminal.
   - Install SBCL using your package manager. On Fedora, run:
     ```bash
     sudo dnf install sbcl
     ```
     On Ubuntu, use:
     ```bash
     sudo apt-get install sbcl
     ```
   - Verify SBCL is installed by running:
     ```bash
     sbcl --version
     ```
     You should see something like `SBCL 2.3.11-4.fc42`.

2. **Install Graphviz (for Neural Diagrams)**:
   - The program uses Graphviz to render diagrams. Install it with:
     ```bash
     sudo dnf install graphviz  # On Fedora
     sudo apt-get install graphviz  # On Ubuntu
     ```
   - Verify Graphviz is installed:
     ```bash
     dot -V
     ```
     You should see the Graphviz version.

3. **Install Quicklisp (to Manage Lisp Libraries)**:
   - Quicklisp is a library manager for Lisp that helps install dependencies like `uiop`, `alexandria`, and `cl-ppcre`.
   - Download Quicklisp:
     ```bash
     curl -O https://beta.quicklisp.org/quicklisp.lisp
     ```
   - Start SBCL:
     ```bash
     sbcl
     ```
   - Load and install Quicklisp in SBCL:
     ```lisp
     (load "quicklisp.lisp")
     (quicklisp-quickstart:install)
     ```
   - Add Quicklisp to your SBCL startup:
     ```lisp
     (ql:add-to-init-file)
     ```
   - Exit SBCL:
     ```lisp
     (sb-ext:exit)
     ```

4. **Save the Program File**:
   - Copy the code from `PeteAI_Groks_Alt_Rlm_LA_SUB_Cltr_Comm_65_Base_Dn_Test_5_23_25.lisp` into a file.
   - Save it to your home directory as `/home/yourusername/PeteAI_Groks_Alt_Rlm_LA_SUB_Cltr_Comm_65_Base_Dn_Test_5_23_25.lisp` (replace `yourusername` with your actual username).

5. **Run the Program**:
   - Start SBCL:
     ```bash
     sbcl
     ```
   - Load the program:
     ```lisp
     (load "/home/yourusername/PeteAI_Groks_Alt_Rlm_LA_SUB_Cltr_Comm_65_Base_Dn_Test_5_23_25.lisp")
     ```
   - The program will start automatically, and you’ll see a prompt:
     ```
     === Pete’s Ready! Say stuff (or 'quit', 'know', 'speak', 'listen', 'read', 'react', 'communicate', 'export', 'neural-diagram', 'clear-memory') ===
     >
     ```
   - You’re now ready to interact with PeteAI! Skip to Section 2 for input strategies.

### For Knowledgeable Users: Quick Setup
If you’re familiar with Lisp and SBCL, here’s a streamlined setup process.

1. **Ensure Dependencies**:
   - Verify SBCL and Graphviz are installed:
     ```bash
     sbcl --version
     dot -V
     ```
   - Ensure Quicklisp is set up and added to your SBCL init file (`~/.sbclrc` should include `(load "~/quicklisp/setup.lisp")`).

2. **Load the Program**:
   - Save the code to `/path/to/PeteAI_Groks_Alt_Rlm_LA_SUB_Cltr_Comm_65_Base_Dn_Test_5_23_25.lisp`.
   - Start SBCL and load the file:
     ```bash
     sbcl --load /path/to/PeteAI_Groks_Alt_Rlm_LA_SUB_Cltr_Comm_65_Base_Dn_Test_5_23_25.lisp
     ```
   - The program will start with the `(converse-no-reset)` prompt:
     ```
     === Pete’s Ready! Say stuff (or 'quit', 'know', 'speak', 'listen', 'read', 'react', 'communicate', 'export', 'neural-diagram', 'clear-memory') ===
     >
     ```

3. **Libraries Used**:
   - The program uses `uiop`, `alexandria`, and `cl-ppcre`, which are loaded via Quicklisp:
     ```lisp
     (ql:quickload :uiop)
     (ql:quickload :alexandria)
     (ql:quickload :cl-ppcre)
     ```
   - It also uses `asdf` (loaded via `(require "asdf")`), which is typically included with SBCL.

## Section 2: Input Strategies at the `>` Prompt

The program starts with `(converse-no-reset)`, which presents an interactive prompt where you can input commands or conversation snippets. Here’s how to use it effectively.

### Basic Commands
At the `>` prompt, you can enter the following commands (type them exactly as shown, without quotes unless specified):
- **quit**: Exits the program.
  ```
  > quit
  ```
  Output: Summarizes the session and exits.
- **know**: Outputs a random piece of knowledge and processes it.
  ```
  > know
  ```
  Output: Something like `Pete knows: stars guide night`, followed by processing.
- **speak**: PeteAI generates a free-form response.
  ```
  > speak
  ```
  Output: PeteAI speaks based on its memory.
- **listen**: Prompts for a conversation input to listen to.
  ```
  > listen
  Enter conversation: stars guide night
  ```
  Output: PeteAI responds to the input.
- **read**: Recalls a memory entry.
  ```
  > read
  ```
  Output: PeteAI recalls a past thought.
- **react**: Reacts to the latest memory.
  ```
  > react
  ```
  Output: PeteAI reacts to its most recent memory.
- **communicate**: Processes two conversation inputs.
  ```
  > communicate
  Enter first convo: stars guide night
  Enter second convo: rivers carve stone
  ```
  Output: PeteAI processes both inputs and visualizes the results.
- **export**: Exports memory to `pete_AI_beast_memory.txt`.
  ```
  > export
  ```
  Output: Saves memory to a file.
- **neural-diagram**: Generates a neural diagram (see Section 4).
  ```
  > neural-diagram
  ```
- **clear-memory**: Resets PeteAI’s memory.
  ```
  > clear-memory
  ```

### Conversation Inputs
You can also type any sentence or phrase directly at the `>` prompt to start a conversation:
```
> The sun ignites the day
```
Output: PeteAI processes the input, updates its memory, and responds with thought trails and treaty deliberations.

**Example Interaction**:
```
> wheels roll forward
Pete splits: (WHEELS ROLL FORWARD)
Pete tags: ((noun WHEELS) (verb ROLL) (verb FORWARD)) (holes: 0)
Pete, depth 0, got: (WHEELS ROLL FORWARD) (mem size: 1, holes: 0)
Wild factor boosted: 2 (holes: 0)
Using root node: (WHEELS ROLL FORWARD)
Thought Trail from WHEELS: ((WHEELS ROLL FORWARD) "
