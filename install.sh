#!/bin/bash

# filepath: /home/abhishekamralkar/.emacs.d/install.sh

# Exit immediately if a command exits with a non-zero status
set -e

echo "Starting Emacs configuration installation..."

# Step 1: Backup existing configuration
echo "Backing up existing Emacs configuration..."
if [ -d "$HOME/.emacs.d" ] || [ -f "$HOME/.emacs" ]; then
    mv "$HOME/.emacs" "$HOME/.emacs.bak" 2>/dev/null || true
    mv "$HOME/.emacs.d" "$HOME/.emacs.d.bak" 2>/dev/null || true
    echo "Backup completed: ~/.emacs.bak and ~/.emacs.d.bak"
else
    echo "No existing configuration found. Skipping backup."
fi

# Step 2: Clone the repository
echo "Cloning the Emacs configuration repository..."
git clone https://github.com/abhishekamralkar/myemacs "$HOME/.emacs.d"

# Step 3: Install dependencies
echo "Installing dependencies..."
if command -v apt >/dev/null; then
    sudo apt update
    sudo apt install -y emacs git wget unzip
elif command -v dnf >/dev/null; then
    sudo dnf install -y emacs git wget unzip
elif command -v brew >/dev/null; then
    brew install emacs git wget unzip
else
    echo "Unsupported package manager. Please install Emacs, Git, wget, and unzip manually."
    exit 1
fi

# Step 4: Check and install FiraCode Nerd Font
echo "Checking for FiraCode Nerd Font..."
if fc-list | grep -q "FiraCode Nerd Font"; then
    echo "FiraCode Nerd Font is already installed."
else
    echo "FiraCode Nerd Font not found. Installing..."
    mkdir -p ~/.local/share/fonts
    cd ~/.local/share/fonts
    wget https://github.com/ryanoasis/nerd-fonts/releases/download/v2.3.3/FiraCode.zip
    unzip -o FiraCode.zip
    fc-cache -fv
    echo "FiraCode Nerd Font installed successfully."
fi

# Step 5: Install fonts for all-the-icons
echo "Installing fonts for all-the-icons..."
emacs --batch -l "$HOME/.emacs.d/init.el" --eval "(all-the-icons-install-fonts t)"

# Step 6: Tangle emacs.org to init.el
echo "Tangling emacs.org to init.el..."
emacs --batch --eval "(progn (find-file \"~/.emacs.d/emacs.org\") (org-babel-tangle) (kill-emacs))"
echo "Tangling completed. init.el generated."

# Step 7: Launch Emacs
echo "Installation complete! Launching Emacs..."
emacs

echo "Enjoy your customized Emacs setup!"