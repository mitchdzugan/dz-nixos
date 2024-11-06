{ lib, pkgs, ... }:

{
  enable = true;
  defaultEditor = true;
  viAlias = true;
  vimAlias = true;
  vimdiffAlias = true;
  withNodeJs = true;
  withPython3 = true;
  withRuby = true;
  extraConfig = builtins.readFile ./init.vim;
  extraLuaConfig = builtins.readFile ./init.lua;
  plugins = let
    nvim-treesitter-with-plugins = pkgs.vimPlugins.nvim-treesitter.withPlugins (treesitter-plugins:
      with treesitter-plugins; [
        bash
        c
        clojure
        cmake
        cpp
        css
        csv
        dhall
        dockerfile
        elixir
        erlang
        gitignore
        graphql
        haskell
        html
        ini
        javascript
        json
        latex
        lua
        luadoc
        make
        markdown
        markdown_inline
        menhir
        nix
        ocaml
        ocaml_interface
        ocamllex
        org
        purescript
        python
        rasi
        regex
        ruby
        rust
        scala
        scss
        sql
        toml
        typescript
        vim
        xml
        yaml
      ]);
    fromGitHub = repo: ref: rev: pkgs.vimUtils.buildVimPlugin {
      pname = "${lib.strings.sanitizeDerivationName repo}";
      version = ref;
      src = builtins.fetchGit {
        url = "https://github.com/${repo}.git";
        ref = ref;
        rev = rev;
      };
    };
  in 
    with pkgs.vimPlugins; [
      dracula-nvim
      gitsigns-nvim
      image-nvim
      indent-blankline-nvim
      lualine-nvim
      mini-icons
      nui-nvim
      nvim-cursorline
      nvim-lspconfig
      nvim-paredit
      nvim-tree-lua
      nvim-treesitter-with-plugins
      nvim-web-devicons
      orgmode
      plenary-nvim
      rose-pine
      rainbow-delimiters-nvim
      tabby-nvim
      telescope-nvim
      venn-nvim
      which-key-nvim
      (fromGitHub 
        "mcauley-penney/tidy.nvim"
        "HEAD"
        "f6c9cfc9ac5a92bb5ba3c354bc2c09a7ffa966f2"
        )
    ];
}