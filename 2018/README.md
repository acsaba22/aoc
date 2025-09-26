# Advent of Code 2018 in Haskell

## Haskel Installation

### Install GHCup

GHCup is the universal installer for the Haskell toolchain.

```bash
# Download and run installer
curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh

# Reload shell environment
source ~/.bashrc
# OR restart your terminal

# Check install size (5G+):
du -sh ~/.ghcup/

ghcup list

# Check versions
ghc --version
cabal --version

# Update package index (like apt update)
cabal update
```

### Complete Removal (Nuclear Option)

```bash
# Remove entire GHCup installation
ghcup nuke

# Remove Cabal global packages (optional)
rm -rf ~/.cabal

# Remove PATH modifications from shell profile
# Edit ~/.bashrc and remove GHCup-related lines
```


### Install VS Code Extensions

Install the official Haskell extension:
- Open VS Code
- Go to Extensions (Ctrl+Shift+X)
- Search for "Haskell" by Haskell Language Server
- Install the extension

## Run code

### Interactive Development

```bash
# Start GHCi (interactive Haskell)
ghci

# Load a file in GHCi
ghci> :load hello.hs
ghci> main

# Exit GHCi
ghci> :quit
```

