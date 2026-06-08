# Doom Emacs Personal Configuration

## Overview

This directory contains the personal Doom Emacs configuration for cashweaver,
written using Org-mode literate programming. The configuration has been
structured modularly, dividing different settings and packages by topic into
separate Org-mode files, which are recursively included into the top-level
`config-personal.org` and tangled to generate the final Emacs Lisp
configuration.

## Files

*   [config-personal.org](file:///google/src/cloud/cashweaver/emacs/google3/experimental/users/cashweaver/dotfiles/files/doom/config-personal.org):
    The top-level personal configuration entry point, utilizing Org mode
    literate programming to include all other modular configuration files.

*   [config-personal.el](file:///google/src/cloud/cashweaver/emacs/google3/experimental/users/cashweaver/dotfiles/files/doom/config-personal.el):
    The main tangled Emacs Lisp output file generated from
    `config-personal.org`.

*   [config-personal-ai.org](file:///google/src/cloud/cashweaver/emacs/google3/experimental/users/cashweaver/dotfiles/files/doom/config-personal-ai.org):
    Personal AI configuration, such as integrations with LLMs or Gemini.

*   [config-personal-app-asana.org](file:///google/src/cloud/cashweaver/emacs/google3/experimental/users/cashweaver/dotfiles/files/doom/config-personal-app-asana.org):
    Modular personal configuration for Asana integration.

*   [config-personal-app-browser.org](file:///google/src/cloud/cashweaver/emacs/google3/experimental/users/cashweaver/dotfiles/files/doom/config-personal-app-browser.org):
    Personal configuration for web browser integration in Emacs.

*   [config-personal-app-calc.org](file:///google/src/cloud/cashweaver/emacs/google3/experimental/users/cashweaver/dotfiles/files/doom/config-personal-app-calc.org):
    Configuration for Emacs Calculator (`calc`).

*   [config-personal-app-calendar.org](file:///google/src/cloud/cashweaver/emacs/google3/experimental/users/cashweaver/dotfiles/files/doom/config-personal-app-calendar.org):
    Personal configuration for Emacs Calendar integration.

*   [config-personal-app-dictionary.org](file:///google/src/cloud/cashweaver/emacs/google3/experimental/users/cashweaver/dotfiles/files/doom/config-personal-app-dictionary.org):
    Configuration for dictionary lookup tools.

*   [config-personal-app-e-reader.org](file:///google/src/cloud/cashweaver/emacs/google3/experimental/users/cashweaver/dotfiles/files/doom/config-personal-app-e-reader.org):
    Configuration for reading electronic books and EPUB files.

*   [config-personal-app-ediff.org](file:///google/src/cloud/cashweaver/emacs/google3/experimental/users/cashweaver/dotfiles/files/doom/config-personal-app-ediff.org):
    Configuration for Ediff, the Emacs interactive diff tool.

*   [config-personal-app-emacs-everywhere.org](file:///google/src/cloud/cashweaver/emacs/google3/experimental/users/cashweaver/dotfiles/files/doom/config-personal-app-emacs-everywhere.org):
    Personal configuration for `emacs-everywhere` to edit system text fields in
    Emacs.

*   [config-personal-app-email.org](file:///google/src/cloud/cashweaver/emacs/google3/experimental/users/cashweaver/dotfiles/files/doom/config-personal-app-email.org):
    Modular personal configuration for email clients (such as Notmuch).

*   [config-personal-app-games.org](file:///google/src/cloud/cashweaver/emacs/google3/experimental/users/cashweaver/dotfiles/files/doom/config-personal-app-games.org):
    Configuration for games and other recreational activities in Emacs.

*   [config-personal-app-gnuplot.org](file:///google/src/cloud/cashweaver/emacs/google3/experimental/users/cashweaver/dotfiles/files/doom/config-personal-app-gnuplot.org):
    Configuration for running Gnuplot inside Emacs.

*   [config-personal-app-image-viewer.org](file:///google/src/cloud/cashweaver/emacs/google3/experimental/users/cashweaver/dotfiles/files/doom/config-personal-app-image-viewer.org):
    Configuration for image viewing options inside Emacs.

*   [config-personal-app-jujutsu.org](file:///google/src/cloud/cashweaver/emacs/google3/experimental/users/cashweaver/dotfiles/files/doom/config-personal-app-jujutsu.org):
    Configuration for the Jujutsu (jj) version control tool in Emacs.

*   [config-personal-app-pandoc.org](file:///google/src/cloud/cashweaver/emacs/google3/experimental/users/cashweaver/dotfiles/files/doom/config-personal-app-pandoc.org):
    Configuration for Pandoc integration.

*   [config-personal-app-rss.org](file:///google/src/cloud/cashweaver/emacs/google3/experimental/users/cashweaver/dotfiles/files/doom/config-personal-app-rss.org):
    Configuration for RSS readers (like Elfeed) in Emacs.

*   [config-personal-app-typing.org](file:///google/src/cloud/cashweaver/emacs/google3/experimental/users/cashweaver/dotfiles/files/doom/config-personal-app-typing.org):
    Personal configuration for typing tutors or practice tools in Emacs.

*   [config-personal-app-vc.org](file:///google/src/cloud/cashweaver/emacs/google3/experimental/users/cashweaver/dotfiles/files/doom/config-personal-app-vc.org):
    Personal configuration for standard Version Control (VC) integration.

*   [config-personal-doom.org](file:///google/src/cloud/cashweaver/emacs/google3/experimental/users/cashweaver/dotfiles/files/doom/config-personal-doom.org):
    Core Doom Emacs personalization settings.

*   [config-personal-elisp.org](file:///google/src/cloud/cashweaver/emacs/google3/experimental/users/cashweaver/dotfiles/files/doom/config-personal-elisp.org):
    Personal configuration and helper functions for Emacs Lisp development.

*   [config-personal-org-agenda.org](file:///google/src/cloud/cashweaver/emacs/google3/experimental/users/cashweaver/dotfiles/files/doom/config-personal-org-agenda.org):
    Personal Org agenda configuration, including files, views, and schedules.

*   [config-personal-org-capture.org](file:///google/src/cloud/cashweaver/emacs/google3/experimental/users/cashweaver/dotfiles/files/doom/config-personal-org-capture.org):
    Personal Org capture templates and setup.

*   [config-personal-org-export.org](file:///google/src/cloud/cashweaver/emacs/google3/experimental/users/cashweaver/dotfiles/files/doom/config-personal-org-export.org):
    Configuration for Org mode export formats and workflows.

*   [config-personal-org-flashcards.org](file:///google/src/cloud/cashweaver/emacs/google3/experimental/users/cashweaver/dotfiles/files/doom/config-personal-org-flashcards.org):
    Org-fc configuration for flashcard-based spaced repetition.

*   [config-personal-org-links.org](file:///google/src/cloud/cashweaver/emacs/google3/experimental/users/cashweaver/dotfiles/files/doom/config-personal-org-links.org):
    Configuration for custom Org link types and hyperlink behaviors.

*   [config-personal-org-roam.org](file:///google/src/cloud/cashweaver/emacs/google3/experimental/users/cashweaver/dotfiles/files/doom/config-personal-org-roam.org):
    Personal Org-roam setup for non-linear slip-box note-taking.

*   [config-personal-org.org](file:///google/src/cloud/cashweaver/emacs/google3/experimental/users/cashweaver/dotfiles/files/doom/config-personal-org.org):
    Central modular configuration for Org-mode, including sub-modules for
    agenda, capture, and roam.

*   [config-personal-pdf.org](file:///google/src/cloud/cashweaver/emacs/google3/experimental/users/cashweaver/dotfiles/files/doom/config-personal-pdf.org):
    Personal configuration for viewing and annotating PDF files.

*   [config-personal-protobuf.org](file:///google/src/cloud/cashweaver/emacs/google3/experimental/users/cashweaver/dotfiles/files/doom/config-personal-protobuf.org):
    Settings for working with Protocol Buffers in Emacs.

*   [config-personal-toml.org](file:///google/src/cloud/cashweaver/emacs/google3/experimental/users/cashweaver/dotfiles/files/doom/config-personal-toml.org):
    Personal configuration for TOML files.
