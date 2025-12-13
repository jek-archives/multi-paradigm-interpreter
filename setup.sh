#!/bin/bash

echo "============================================"
echo "    Multi-Paradigm Interpreter Setup"
echo "============================================"

# Check for Homebrew
if ! command -v brew &> /dev/null; then
    echo "❌ Homebrew is NOT installed."
    echo "   You need Homebrew to install the required compilers easily."
    echo ""
    echo "   ► To install Homebrew, run this command used by millions of developers:"
    echo '     /bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"'
    echo ""
    echo "   After installing, restart your terminal and run this script again."
    exit 1
else
    echo "✅ Homebrew is installed."
fi

echo "--------------------------------------------"
echo "Checking Language Dependencies..."

# Check Prolog
if ! command -v swipl &> /dev/null; then
    echo "❌ SWI-Prolog is missing."
    echo "   Installing swi-prolog..."
    brew install swi-prolog
else
    echo "✅ SWI-Prolog is found."
fi

# Check Haskell
if ! command -v ghc &> /dev/null; then
    echo "❌ GHC (Haskell) is missing."
    echo "   Installing ghc..."
    brew install ghc
else
    echo "✅ GHC (Haskell) is found."
fi

# Check Python
if ! command -v python3 &> /dev/null; then
    echo "❌ Python3 is missing (Unexpected on macOS!)."
else
    echo "✅ Python3 is found."
fi

# Check GCC
if ! command -v gcc &> /dev/null; then
    echo "❌ GCC is missing."
    echo "   Running xcode-select --install..."
    xcode-select --install
else
    echo "✅ GCC is found."
fi

echo "============================================"
echo "Setup Check Complete."
echo ""
echo "To build the project:"
echo "  make"
echo ""
echo "To run the Web GUI:"
echo "  python3 gui/server.py"
echo "============================================"
