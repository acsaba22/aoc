# Setup

## Install java

Check that `java -version` and `javac -version` return the same version which is >= 17.

For example:

```
sudo apt install openjdk-22-jdk
```

## Install Kotlin command line compiler

https://kotlinlang.org/docs/command-line.html

For example:

```
cd ~
wget https://github.com/JetBrains/kotlin/releases/download/v1.9.21/kotlin-compiler-1.9.21.zip
unzip kotlin-compiler-1.9.21.zip
# add $HOME/kotlinc/bin to PATH
```

## VS Code extensions

Add the VS Code extension pack: https://marketplace.visualstudio.com/items?itemName=sethjones.kotlin-on-vscode

Press `Ctrl-P` and enter `ext install sethjones.kotlin-on-vscode`

Now you can build and run `*.kt` files with the "run triangle button" in the top-right corner.

If you prefer to do it manually:

```
cd day00
kotlinc main.kt -include-runtime -d main.jar
java -jar main.jar
```
