#!/bin/bash

# Define paths
SOURCE_DIR="$(pwd)/lambda-extension"
DEST_DIR="$HOME/.vscode/extensions/lambda-language"

echo "Installing Lambda extension..."
echo "Source: $SOURCE_DIR"
echo "Destination: $DEST_DIR"

# Check if source exists
if [ ! -d "$SOURCE_DIR" ]; then
    echo "Error: Source directory $SOURCE_DIR does not exist."
    exit 1
fi

# Remove old version if it exists
if [ -d "$DEST_DIR" ]; then
    echo "Removing old extension version..."
    rm -rf "$DEST_DIR"
fi

# Create destination directory
mkdir -p "$DEST_DIR"

# Copy files
cp -r "$SOURCE_DIR/"* "$DEST_DIR/"

echo "------------------------------------------------"
echo "Extension installed successfully!"
echo "Please RESTART VS Code (or Reload Window) to activate the syntax highlighting."
echo "------------------------------------------------"
