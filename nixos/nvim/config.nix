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
  extraLuaConfig = ''require("dz.init")'';
  plugins = let
    markdownWithQueries = pkgs.vimPlugins.nvim-treesitter-parsers.markdown.overrideAttrs { installQueries = true; };
    orgWithQueries = pkgs.vimPlugins.nvim-treesitter-parsers.org.overrideAttrs { installQueries = true; };
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
        markdownWithQueries
        markdown_inline
        menhir
        nix
        ocaml
        ocaml_interface
        ocamllex
        orgWithQueries
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
      catppuccin-nvim
      cmp-nvim-lsp
      cmp-buffer
      cmp-path
      cmp-cmdline
      cmp-vsnip
      dracula-nvim
      gitsigns-nvim
      guess-indent-nvim
      image-nvim
      indent-blankline-nvim
      lualine-nvim
      mini-icons
      netrw-nvim
      nui-nvim
      nvim-cmp
      nvim-cursorline
      nvim-lspconfig
      nvim-paredit
      nvim-tree-lua
      nvim-treesitter-with-plugins
      nvim-web-devicons
      plenary-nvim
      render-markdown-nvim
      rose-pine
      rainbow-delimiters-nvim
      tabby-nvim
      telescope-nvim
      venn-nvim
      vim-vsnip
      which-key-nvim
      (fromGitHub 
        "mcauley-penney/tidy.nvim"
        "HEAD"
        "f6c9cfc9ac5a92bb5ba3c354bc2c09a7ffa966f2"
        )
      (fromGitHub 
        "Jxstxs/conceal.nvim"
        "HEAD"
        "1aff9fc5d1157aef1c7c88b6df6d6db21268d00a"
        )
      (fromGitHub 
        "tiagovla/tokyodark.nvim"
        "HEAD"
        "14bc1b3e596878a10647af7c82de7736300f3322"
        )
      (fromGitHub 
        "bluz71/vim-moonfly-colors"
        "HEAD"
        "63f20d657c9fd46ecdd75bd45c321f74ef9b11fe"
        )
    ];
}