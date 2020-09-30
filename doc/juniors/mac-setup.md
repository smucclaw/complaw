# Macbook Air Setup

This is a brief guide on setting up the development environment on SMU-issued Macbook Airs for first-time Mac users. 

Feel free to approach any contributer of this page (listed above on the github page) for help! 

-----------------

## Prerequisite Installation

### Xcode CLI tools

xcode-select is a command-line utility on macOS that functions as a "train conductor" when we invoke command like `make`, `xcodebuild` and other compilation applications. [1]

Opening a terminal window, run the following command.

```zsh
xcode-select --install
```

Alternatively, you can choose to install Xcode from Mac's app store instead if you prefer. 



## Package Managers

### Homebrew

https://brew.sh/

This is the first thing you'll need to install. Homebrew is a package manager for macOS that makes it easy to install, update and remove applications and utilities on a Mac. 

The landing page of the website lists the instructions, and gives an overview of what it can do.

Once your installation of homebrew is complete, run the following commands to ensure that Homebrew is installed correctly, and all available packages are up to date.

```zsh
brew doctor
brew update
```

 

### Stack

```zsh
brew install haskell-stack
```

Once you've installed homebrew, you can start using it to install Stack. Stack is a project-manager for Haskell. It installs the Haskell compiler, GHC (either globally or isolated on a per-project basis), builds your projects with the packages required, and allows you to run tests too!

This should be relatively straightforward with brew, but you might want to test your stack installation by going through the quickstart guide here: https://docs.haskellstack.org/en/stable/README/#quick-start-guide





## Applications

### Git

```zsh
brew install git
```

You'll have to set up your git config after. (Hopefully this isn't your first rodeo with git). 

The specifics of getting your git setup to work with the remote repositiories on CCLAW's github can be found here (under SSH & Git) : https://github.com/smucclaw/complaw/blob/primary/doc/juniors/recommended-configs.MD 



### Grammatical Framework (GF)

Where possible, try to download the binaries for macOS off the GF webpage: https://www.grammaticalframework.org/download/

If for some reason, the installation is blocked, these are the instructions for vanilla GF from scratch.

1. Clone the `gf-core` repository from https://github.com/GrammaticalFramework

2. `cd` into the `gf-core` folder, and run `stack install` 

3. When the installation is complete, run the following command: 

   ```zsh
   echo 'export PATH=$HOME'/.local/bin:'$PATH' >> $HOME/.zshrc ; 
   source ~/.zshrc
   ```

4. At this point, GF (without RGL) should be installed. You can test it by running `gf` in your terminal window.

To install the GF Resource Grammar Libraries (RGL) from source code, 

1. Clone the  `gf-rgl` repository from https://github.com/GrammaticalFramework

   cd into the `gf-rgl` folder, and run `./Setup.sh`

2. When the installation is complete, run the following command: `

   ```zsh
   echo 'export GF_LIB_PATH='"$PWD"/dist:'$GF_LIB_PATH' >> $HOME/.zshrc
   source ~/.zshrc
   ```

3. At this point, RGL should be installed. 





## References

​	[1] https://macops.ca/developer-binaries-on-os-x-xcode-select-and-xcrun/

​	[2] https://cscheng.info/2017/01/26/git-tip-autostash-with-git-pull-rebase.html

