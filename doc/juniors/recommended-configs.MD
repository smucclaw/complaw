# Recommended configs and tools

#### Support on CCLAW slack

\#tools #windows (rip Windows users) #rubberduck (general problem-solving channel)

@2jacobtan for feedback / help.

---

# Windows Subsystem for Linux (WSL)

On a Windows PC, this is essential for Haskell and Grammatical Framework development. Things do not work properly on native Windows.

Installation https://docs.microsoft.com/en-us/windows/wsl/install-win10

Recommended Linux distro: Ubuntu 20.04 LTS (with the version number in the name; the one without version number will auto-upgrade to a newer Ubuntu version when it comes out 2 years later, which may cause compatibility problems)
https://www.microsoft.com/en-sg/p/ubuntu-2004-lts/9n6svws3rx71?rtc=1&activetab=pivot:overviewtab

Linux command line tutorial https://missing.csail.mit.edu/2020/course-shell/

#### misc.

- SSH Key Generation https://ubuntu.com/tutorials/ssh-keygen-on-windows#3-key-generation-with-ubuntu-on-wsl

---

# VS Code

https://code.visualstudio.com/download

### Extensions

Follow instructions for each Haskell-specific extension to install properly:
- https://github.com/haskell/vscode-haskell
- https://github.com/ndmitchell/hlint

These should work out-of-the-box:
- Remote WSL https://marketplace.visualstudio.com/items?itemName=ms-vscode-remote.remote-wsl
- Remote SSH https://marketplace.visualstudio.com/items?itemName=ms-vscode-remote.remote-ssh
  
### Useful features

- Tips and Tricks https://code.visualstudio.com/docs/getstarted/tips-and-tricks
- Code snippets https://code.visualstudio.com/docs/editor/userdefinedsnippets

### Cosmetic
Hasklig font (designed for Haskell) https://github.com/i-tu/Hasklig

Nice theme https://marketplace.visualstudio.com/items?itemName=monokai.theme-monokai-pro-vscode

---

# SSH

Adding a new SSH key to your GitHub account https://docs.github.com/en/github/authenticating-to-github/adding-a-new-ssh-key-to-your-github-account

Set up an SSH key https://support.atlassian.com/bitbucket-cloud/docs/set-up-an-ssh-key/#SetupanSSHkey-ssh1

---

# Haskell

Haskell Stack https://docs.haskellstack.org/en/stable/install_and_upgrade/

ghcup https://www.haskell.org/ghcup/

---

# Grammatical Framework

https://www.grammaticalframework.org/download/

Follow instructions with these headers (ctrl+f):
- Haskeline
- Installing from the latest developer source code
- Installing the RGL from source

VS Code extension (syntax highlighting) https://marketplace.visualstudio.com/items?itemName=robclouth.gf-vscode

---

# Nix and NixOS

A kind of package manager used with (hence needed by) our Haskell projects.

https://nixos.org/download.html

### When used with Haskell Stack

https://docs.haskellstack.org/en/stable/nix_integration/#additions-to-your-stackyaml

Main points to note:

- Use `stack --nix build` instead of plain `stack build` to ensure Nix is activated.
- Add these lines to the `stack.yaml` file:
```
nix:
  enable: true
```

---

# Misc.

#### Microsoft Outlook (SMU email)

To turn off inconvenient default behaviour:

- How To Prevent Outlook From Deleting Meeting Request When Responding? https://www.extendoffice.com/documents/outlook/1948-prevent-outlook-from-deleting-meeting-requests.html
