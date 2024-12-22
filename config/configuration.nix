{ config, pkgs, lib, hostname, home-manager,
  hyprland, hyprland-plugins, hyprland-hyprfocus, hyprland-dyncursors,
  wezterm-flake, ssbm, sddm-dz, nur, ...
}: let
  nixospkgs = pkgs;
  lg_hdmi_fingerprint = "00ffffffffffff001e6d6e77f48c0400041f010380462778ea8cb5af4f43ab260e5054210800d1c06140010101010101010101010101e9e800a0a0a0535030203500b9882100001a000000fd0030901ee63c000a202020202020000000fc004c4720554c545241474541520a000000ff003130344e544a4a38533232380a01b8020349f1230907074d100403011f13123f5d5e5f60616d030c002000b83c20006001020367d85dc401788003e30f00186d1a00000205309000045a445a44e305c000e60605015a5a446fc200a0a0a0555030203500b9882100001a5aa000a0a0a0465030203a00b9882100001a565e00a0a0a0295030203500b9882100001aed";
  acer_vga_fingerprint = "00ffffffffffff0004726f04c33a1060011a010368351e78ee0565a756529c270f5054b30c00714f818081c081009500b300d1c00101023a801871382d40582c45000f282100001e000000fd00384b1f4b12000a202020202020000000fc005232343048590a202020202020000000ff005434424141303031323430300a003a";
  acer_hdmi_fingerprint = "00ffffffffffff0004726f04c33a1060011a010380351e78ee0565a756529c270f5054b30c00714f818081c081009500b300d1c00101023a801871382d40582c45000f282100001e000000fd00384b1f4b12000a202020202020000000fc005232343048590a202020202020000000ff005434424141303031323430300a012102031cf1499001030412131f0514230907078301000065030c001000023a801871382d40582c45000f282100001e011d007251d01e206e2855000f282100001e8c0ad08a20e02d10103e96000f2821000018d60980a020e02d10086022000f28210808180000000000000000000000000000000000000000000000000000002e";
  gazelle_fingerprint = "00ffffffffffff0030e44d0700000000001f0104a52213780371c5985e5b8f271b5054000000010101010101010101010101010101015f8780a07038a0463020350058c21000001a000000fd003c90a7a723010a202020202020000000fe004c4720444953504c41590a2020000000fe004c5031353657464a2d53504234014270137900000301145e8700847f079f002f801f0037044c000200040000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000007390";
  hp_fingerprint = "00ffffffffffff0006af3d4000000000101d0104951f1178029b85925659902920505400000001010101010101010101010101010101663a80b87038684010103e0035ae10000018ef2680b87038684010103e0035ae1000001800000000000000000000000000000000000000000002001048ff0f3c7d490d1b7d202020006f";
  autorandr_profiles = {
    "hp_solo" = {
      fingerprint = {
        "eDP-1" = hp_fingerprint;
      };
      config = {
        "eDP-1" = {
          enable = true;
          primary = false;
          position = "0x0";
          mode = "1920x1080";
          rate = "60.01";
          dpi = 96;
        };
      };
    };
    "gazelle_solo" = {
      fingerprint = {
        "eDP-1" = gazelle_fingerprint;
      };
      config = {
        "eDP-1" = {
          enable = true;
          primary = false;
          position = "0x0";
          mode = "1920x1080";
          rate = "144.00";
          dpi = 96;
        };
      };
    };
    "gazelle_dual" = {
      fingerprint = {
        "eDP-1" = gazelle_fingerprint;
        "HDMI-1" = acer_hdmi_fingerprint;
      };
      config = {
        "eDP-1" = {
          enable = true;
          primary = false;
          position = "0x0";
          mode = "1920x1080";
          rate = "144.00";
          dpi = 96;
        };
        "HDMI-1" = {
          enable = true;
          primary = true;
          position = "1920x0";
          mode = "1920x1080";
          rate = "60.00";
          dpi = 96;
        };
      };
    };
    "solo_acer" = {
      fingerprint = {
        "VGA-0" = acer_vga_fingerprint;
      };
      config = {
        "VGA-0" = {
          enable = true;
          primary = false;
          position = "0x0";
          mode = "1920x1080";
          dpi = 96;
        };
      };
    };
    "solo_lg" = {
      fingerprint = {
        "HDMI-0" = lg_hdmi_fingerprint;
      };
      config = {
        "HDMI-0" = {
          enable = true;
          primary = true;
          position = "0x0";
          mode = "1920x1080";
          rate = "119.88";
          dpi = 96;
        };
      };
    };
    "dual" = {
      fingerprint = {
        "HDMI-0" = lg_hdmi_fingerprint;
        "VGA-0" = acer_vga_fingerprint;
      };
      config = {
        "VGA-0" = {
          enable = true;
          primary = false;
          position = "0x0";
          mode = "1920x1080";
          dpi = 96;
        };
        "HDMI-0" = {
          enable = true;
          primary = true;
          position = "1920x0";
          mode = "1920x1080";
          rate = "119.88";
          dpi = 96;
        };
      };
    };
  };
  byHostname = {
    mitch-desktop = {
      musicDir = "/VOID/Media/Music";
      kernalPackages = pkgs.linuxPackages_rt_5_10;
      bspwmExtraConfig = ''
      '';
      videoDrivers = ["nvidia"];
      nvidia = {
        modesetting.enable = true;
        powerManagement.finegrained = false;
        open = false;
        nvidiaSettings = true;
        package = config.boot.kernelPackages.nvidiaPackages.legacy_470;
      };
    };
    mitch-hp = {
      musicDir = "/Music";
      kernalPackages = pkgs.linuxPackages_xanmod_latest;
      bspwmExtraConfig = ''
xinputSetTouchpadNaturalScroll
xinputSetTouchpadTapping
      '';
      videoDrivers = ["modesetting" "fbdev"];
      nvidia = {};
    };
    mitch-gazelle = {
      musicDir = "/Music";
      kernalPackages = pkgs.linuxPackages_xanmod_latest;
      bspwmExtraConfig = ''
xinputSetTouchpadNaturalScroll
      '';
      videoDrivers = ["nvidia"];
      nvidia = {
        modesetting.enable = true;
        powerManagement.finegrained = false;
        open = false;
        nvidiaSettings = true;
        package = config.boot.kernelPackages.nvidiaPackages.beta;
        prime = {
          # sync.enable = true;
          offload = {
            enable = true;
            enableOffloadCmd = true;
          };
          # Make sure to use the correct Bus ID values for your system!
          nvidiaBusId = "PCI:1:0:0";
          intelBusId = "PCI:0:2:0";
        };
      };
    };
  };
  musicDir = byHostname.${hostname}.musicDir;
  mk_xmolib = haskellPackages: haskellPackages.mkDerivation {
    pname = "xmolib";
    version = "0.0.1";
    src = ./domain/xmolib;
    libraryHaskellDepends = with haskellPackages; [
      base prettyprinter prettyprinter-ansi-terminal process text
      transformers transformers-compat
      aeson xmonad xmonad-contrib xmonad-dbus xmonad-extras
    ];
    license = lib.licenses.bsd3;
  };
  dz_xonsh = pkgs.xonsh.override {
    extraPackages = ps: [
      ps.coconut
      ps.pip
    ];
  };
in {
  imports =
    [ # Include the results of the hardware scan.
      ./hardware-configuration.nix
      home-manager.nixosModules.default
    ];

  nix.settings.experimental-features = [ "nix-command" "flakes" ];

  # Bootloader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;
  boot.kernelPackages = byHostname.${hostname}.kernalPackages;
  boot.extraModulePackages = with config.boot.kernelPackages; [ gcadapter-oc-kmod ];
  boot.kernelModules = [ "gcadapter-oc" ];
  boot.kernelParams = [ "threadirqs" ];

  fileSystems."/VOID" = lib.mkIf (hostname == "mitch-desktop") {
    device = "/dev/disk/by-uuid/2f9c67b5-e2d4-4ddf-a3be-496255b3fb16";
    fsType = "ext4";
  };

  networking.hostName = hostname; # Define your hostname.
  # networking.wireless.enable = true;  # Enables wireless support via wpa_supplicant.

  # Configure network proxy if necessary
  # networking.proxy.default = "http://user:password@proxy:port/";
  # networking.proxy.noProxy = "127.0.0.1,localhost,internal.domain";

  # Enable networking
  networking.networkmanager.enable = true;

  # Set your time zone.
  time.timeZone = "America/Chicago";

  # Select internationalisation properties.
  i18n.defaultLocale = "en_US.UTF-8";

  i18n.extraLocaleSettings = {
    LC_ADDRESS = "en_US.UTF-8";
    LC_IDENTIFICATION = "en_US.UTF-8";
    LC_MEASUREMENT = "en_US.UTF-8";
    LC_MONETARY = "en_US.UTF-8";
    LC_NAME = "en_US.UTF-8";
    LC_NUMERIC = "en_US.UTF-8";
    LC_PAPER = "en_US.UTF-8";
    LC_TELEPHONE = "en_US.UTF-8";
    LC_TIME = "en_US.UTF-8";
  };

  # Enable the X11 windowing system.
  services.xserver.enable = true;

  # Enable the KDE Plasma Desktop Environment.
  services.displayManager.sddm.enable = true;
  services.displayManager.sddm.theme = "sddm-dz";
  services.displayManager.sddm.settings = {
    Theme = {
      CursorSize = 32;
      CursorTheme = "BreezeX-RosePineDawn-Linux";
    };
  };
  services.xserver.displayManager.setupCommands = "${pkgs.autorandr}/bin/autorandr -c";
  systemd.services."sddm-avatar" = {
    description = "Service to copy or update users Avatars at startup.";
    wantedBy = [ "multi-user.target" ];
    before = [ "sddm.service" ];
    script = ''
      set -eu
      for user in /home/*; do
          username=$(basename "$user")
          if [ -f "$user/.face.icon" ]; then
              if [ ! -f "/var/lib/AccountsService/icons/$username" ]; then
                  cp "$user/.face.icon" "/var/lib/AccountsService/icons/$username"
              else
                  if [ "$user/.face.icon" -nt "/var/lib/AccountsService/icons/$username" ]; then
                      cp "$user/.face.icon" "/var/lib/AccountsService/icons/$username"
                  fi
              fi
          fi
      done
    '';
    serviceConfig = {
      Type = "simple";
      User = "root";
      StandardOutput = "journal+console";
      StandardError = "journal+console";
    };
  };
  systemd.services.sddm = { after = [ "sddm-avatar.service" ]; };
  services.xserver.windowManager.xmonad = {
    enable = true;
    enableContribAndExtras = true;
    extraPackages = haskellPackages: [
      haskellPackages.dbus
      haskellPackages.List
      haskellPackages.monad-logger
      (mk_xmolib haskellPackages)
    ];
  };
  services.xserver.windowManager.bspwm.enable = true;
  services.xserver.desktopManager.gnome.enable = true;
  programs.hyprland = {
    enable = true;
    package = hyprland.packages.${pkgs.stdenv.hostPlatform.system}.hyprland;
    portalPackage =
      hyprland.packages.${pkgs.stdenv.hostPlatform.system}.xdg-desktop-portal-hyprland;
  };

  # Configure keymap in X11
  services.xserver.xkb = {
    layout = "us";
    variant = "";
  };

  # Enable CUPS to print documents.
  services.printing.enable = true;

  #### # Enable sound with pipewire.
  #### hardware.pulseaudio.enable = false;
  #### security.rtkit.enable = true;
  services.pipewire = {
    enable = true;
    audio.enable = false;
    alsa.enable = false;
    alsa.support32Bit = false;
    pulse.enable = false;
    #### # If you want to use JACK applications, uncomment this
    #### #jack.enable = true;
####
    #### # use the example session manager (no others are packaged yet so this is enabled by default,
    #### # no need to redefine it in your config for now)
    #### #media-session.enable = true;
  };

  hardware.pulseaudio.enable = true;
  hardware.pulseaudio.support32Bit = true;
  hardware.pulseaudio.extraConfig = "load-module module-native-protocol-tcp auth-ip-acl=127.0.0.1";
  # hardware.pulseaudio.configFile = pkgs.runCommand "default.pa" {} ''
  # sed 's/module-udev-detect$/module-udev-detect tsched=0/' \
    # ${pkgs.pulseaudio}/etc/pulse/default.pa > $out
  # '';
  hardware.pulseaudio.daemon.config = {
    flat-volumes= "no";
    high-priority = "yes";
    realtime-scheduling = "yes";
    resample-method = "speex-float-0";
    default-fragments = 2;
    default-fragment-size-msec = 2;
  };
  nixpkgs.config.pulseaudio = true;
  security.rtkit.enable = true;

  hardware.bluetooth.enable = true;
  hardware.bluetooth.powerOnBoot = true;
  services.blueman.enable = true;

  # Enable touchpad support (enabled default in most desktopManager).
  # services.xserver.libinput.enable = true;

  services.udev.extraRules = ''
ATTRS{idVendor}=="057e", ATTRS{idProduct}=="0337", MODE="666", SUBSYSTEM=="usb", ENV{DEVTYPE}=="usb_device" TAG+="uaccess"
ACTION=="change", SUBSYSTEM=="drm", RUN+="${pkgs.autorandr}/bin/autorandr -c"
  '';

  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.users.dz = {
    isNormalUser = true;
    description = "Mitch Dzugan";
    extraGroups = [ "audio" "networkmanager" "wheel" ];
    packages = with pkgs; [ ];
    shell = pkgs.bash;
  };

  home-manager.users.dz = hm@{ pkgs, ... }: {
    home = {
      pointerCursor = {
        gtk.enable = true;
        x11.enable = true;
        # package = pkgs.catppuccin-cursors.mochaRosewater; # bibata-cursors;
        package = pkgs.rose-pine-cursor;
        name = "BreezeX-RosePineDawn-Linux";
        size = 32;
        hyprcursor = { enable = true; size = 32; };
      };
      packages = [ pkgs.gimp pkgs.mpv ];
      ## sessionVariables.QT_QPA_PLATFORM = "wayland";
      sessionPath = [
        "/home/dz/Projects/dz-bin"
        "/home/dz/Projects/dz-bspwm/bin"
      ];
    };

    gtk.enable = true;
    gtk.theme.package = pkgs.rose-pine-gtk-theme;
    gtk.theme.name = "rose-pine";
    gtk.iconTheme.package = pkgs.dracula-icon-theme;
    gtk.iconTheme.name = "Dracula";

    xdg.configFile = {
      "blesh" = {
        source = hm.config.lib.file.mkOutOfStoreSymlink ./domain/bash/blesh;
        recursive = true;
      };
      "fastfetch" = {
        source = hm.config.lib.file.mkOutOfStoreSymlink ./domain/fastfetch;
        recursive = true;
      };
      "nvim/lua" = {
        source = hm.config.lib.file.mkOutOfStoreSymlink ./domain/nvim/lua;
        recursive = true;
      };
      "xmonad/xmonad.hs" = {
        source = pkgs.writeText "xmonad.hs" ''
          import qualified Xmolib.Entry.Xmonad as Xmolib
          main :: IO ()
          main = Xmolib.runXmonad
        '';
        recursive = false;
      };
      "xonsh/rc.d" = {
        source = hm.config.lib.file.mkOutOfStoreSymlink ./domain/xonsh/rc.d;
        recursive = true;
      };
    };

    programs = {
      autorandr = {
        enable = true;
        profiles = autorandr_profiles;
      };

      bash = {
        enable = true;
        # enableCompletion = true;
        # bashrcExtra = builtins.readFile ./domain/bash/bashrcExtra.sh;
        # initExtra = builtins.readFile ./domain/bash/initExtra.sh;
      };

      fish = {
        enable = true;
        functions = {
          get_fish_pid = "echo $fish_pid";
          get_fish_pid_interactive = "echo $fish_pid_interactive";
          cdproj = "cd $(codeProject.py)";
          _zdev_is_active = ''
            if [ "$ZDEV_ACTIVE" != "1" ]
              return 1
            end
            set -l pidvar "ZDEV_PID_$ZDEV_ID"
            if [ "$$pidvar" != "$fish_pid_interactive" ]
              return 1
            end
          '';
          _tide_item_znix = ''
            if set -q IN_NIX_SHELL
              set -l depth "?"
              set -l label $IN_NIX_SHELL
              if _zdev_is_active
                set depth $ZDEV_DEPTH
                set label $ZDEV_LABEL
              end
              set label (set_color $tide_znix_color_bright && echo $label)
              set -l full $label
              if [ "$depth" != "1" ]
                set -l sep (set_color $tide_znix_color && echo ":")
                set depth (set_color $tide_znix_color_bright && echo $depth)
                set full $label$sep$depth
              end
              _tide_print_item znix "$tide_znix_icon $full"
            end
          '';
          _tide_item_rich_status = ''
            if string match -qv 0 $_tide_pipestatus # If there is a failure anywhere in the pipestatus
              fish_status_to_signal $_tide_pipestatus | string replace SIG "" | string join '|' | read -l out
              test $_tide_status = 0 && _tide_print_item rich_status $tide_rich_status_icon' ' $out ||
                tide_rich_status_bg_color=$tide_rich_status_bg_color_failure tide_rich_status_color=$tide_rich_status_color_failure \
                  _tide_print_item rich_status $tide_rich_status_icon_failure' ' $out
            else if not contains zpb $_tide_left_items
              _tide_print_item rich_status $tide_rich_status_icon
            end
          '';
          _tide_item_rich_character = ''
            test $_tide_status = 0 \
              && set -fx tide_rich_character_bg_color $tide_rich_character_bg0 \
              || set -fx tide_rich_character_bg_color $tide_rich_character_bgX
            test $_tide_status = 0 \
              && set -fx tide_rich_character_color $tide_rich_character_color0 \
              || set -fx tide_rich_character_color $tide_rich_character_colorX

            _tide_print_item rich_character $tide_rich_character_char
          '';
          _tide_item_zvi_mode = ''
            _tide_item_vi_mode
          '';
          _tide_item_zpb = ''
            set -gx tide_left_prompt_separator_diff_color "🭎"
            _tide_item_rich_character
          '';
          _tide_item_zpp = ''
            set -gx tide_left_prompt_separator_diff_color "🭪"
            _tide_item_rich_character
          '';
          _tide_item_zpe = ''
            set -l prev_sep $tide_right_prompt_separator_diff_color
            set -gx tide_right_prompt_separator_diff_color "🮍"
            _tide_item_rich_character
            set -gx tide_right_prompt_separator_diff_color $prev_sep
          '';
          _tide_item_zpwd = ''
            set -gx tide_left_prompt_separator_diff_color "🬾"
            _tide_item_pwd
          '';
          _tide_item_rich_context = ''
            set -l ctxt_icon ""
            if set -q SSH_TTY
              set -fx tide_rich_context_color $tide_rich_context_color_ssh
              set ctxt_icon "󰌘"
            else if test "$EUID" = 0
              set -fx tide_rich_context_color $tide_rich_context_color_root
              set ctxt_icon "󰞀"
            else if test "$tide_rich_context_always_display" = true
              set -fx tide_rich_context_color $tide_rich_context_color_default
              set ctxt_icon "󰍹"
            else
              return
            end

            string match -qr "^(?<h>(\.?[^\.]*){0,$tide_context_hostname_parts})" $hostname
            set -l fullstr "$ctxt_icon $(\
              set_color $tide_rich_context_color_user && echo $USER)$(\
              set_color $tide_rich_context_color && echo @)$(\
              set_color $tide_rich_context_color_host && echo $h)"
            _tide_print_item rich_context $fullstr
          '';
        };
        interactiveShellInit = builtins.readFile (
          ./domain/fish/interactiveShellInit.fish
        );
        plugins = with pkgs.fishPlugins; [
          { name = "z"; src = z.src; }
          { name = "grc"; src = grc.src; }
          { name = "fzf"; src = fzf.src; }
          { name = "tide"; src = tide.src; }
          { name = "done"; src = done.src; }
          { name = "bass"; src = bass.src; }
          { name = "gruvbox"; src = gruvbox.src; }
          { name = "autopair"; src = autopair.src; }
        ];
      };

      kitty = {
        enable = true;
        shellIntegration = {
          enableFishIntegration = true;
        };
        settings = {
          shell = "fish";
          confirm_os_window_close = -1;
          cursor_trail = 1;
          cursor_blink_interval = "1.0 ease-in";
          dynamic_background_opacity = "yes";
          background_opacity = 0.9;
          transparent_background_colors = lib.concatStrings [
            "#604b49@0.9 "
            "#605955@0.9 "
            "#385167@0.9 "
            "#4b4e6c@0.9 "
            "#11111b@0.8 "
            "#6c7086@0.8 "
            "#181825@0.8 "
          ];
        };
        themeFile = "purpurite";
      };

      alacritty = {
        enable = true;
        settings = {
          env.WINIT_X11_SCALE_FACTOR = "1.0";
          window.opacity = 0.85;
          colors.transparent_background_colors = true;
          font.size = 11.0;
          font.normal = { family = "MonaspiceKr Nerd Font Mono"; style = "Medium"; };
          colors.primary = {
            foreground = "#e0def4";
            background = "#191724";
            dim_foreground = "#908caa";
            bright_foreground = "#e0def4";
          };

          colors.cursor = {
            text = "#e0def4";
            cursor = "#524f67";
          };

          colors.vi_mode_cursor = {
            text = "#e0def4";
            cursor = "#524f67";
          };

          colors.search.matches = {
            foreground = "#908caa";
            background = "#26233a";
          };

          colors.search.focused_match = {
            foreground = "#191724";
            background = "#ebbcba";
          };

          colors.hints.start = {
            foreground = "#908caa";
            background = "#1f1d2e";
          };

          colors.hints.end = {
            foreground = "#6e6a86";
            background = "#1f1d2e";
          };

          colors.line_indicator = {
            foreground = "None";
            background = "None";
          };

          colors.footer_bar = {
            foreground = "#e0def4";
            background = "#1f1d2e";
          };

          colors.selection = {
            text = "#e0def4";
            background = "#403d52";
          };

          colors.normal = {
            black = "#26233a";
            red = "#eb6f92";
            green = "#31748f";
            yellow = "#f6c177";
            blue = "#9ccfd8";
            magenta = "#c4a7e7";
            cyan = "#ebbcba";
            white = "#e0def4";
          };

          colors.bright = {
            black = "#6e6a86";
            red = "#eb6f92";
            green = "#31748f";
            yellow = "#f6c177";
            blue = "#9ccfd8";
            magenta = "#c4a7e7";
            cyan = "#ebbcba";
            white = "#e0def4";
          };

          colors.dim = {
            black = "#6e6a86";
            red = "#eb6f92";
            green = "#31748f";
            yellow = "#f6c177";
            blue = "#9ccfd8";
            magenta = "#c4a7e7";
            cyan = "#ebbcba";
            white = "#e0def4";
          };
        };
      };

      git = {
        enable = true;
        userName  = "Mitch Dzugan";
        userEmail = "mitchdzugan@gmail.com";
        extraConfig = {
          init.defaultBranch = "main";
          pull.rebase = false;
        };
      };

      direnv = {
        enable = true;
        # enableBashIntegration = true;
        # enableZshIntegration = true;
        nix-direnv.enable = true;
      };

      wezterm = {
        enable = true;
        package = wezterm-flake.packages."${pkgs.system}".default;
        enableBashIntegration = true;
        extraConfig = ''
          return {
            color_scheme = "catppuccin-mocha",
            font = wezterm.font_with_fallback({
              "MonaspiceKr Nerd Font Mono",
              "ComicShannsMono Nerd Font Mono",
              "Monaspace Krypton",
              "Ubuntu Mono derivative Powerline",
            }),
            hide_tab_bar_if_only_one_tab = true,
            window_background_opacity = 0.85,
            text_background_opacity = 1.00,
            font_size = 11.0,
            window_padding = { top = 0, left = 0, right = 0, bottom = 0 },
            freetype_load_flags = 'NO_HINTING',
            unix_domains = {{ name = 'unix' }},
          }
        '';
      };

      neovim = import ./domain/nvim/config.nix { lib = lib; pkgs = pkgs; };
      # neovide = import ./domain/nvim/neovide.nix;

      tmux = {
        enable = true;
        clock24 = true;
        mouse = true;
        escapeTime = 0;
        keyMode = "vi";
        plugins = with pkgs.tmuxPlugins; [
          {
            plugin = rose-pine;
            extraConfig = ''
              set -g @rose_pine_variant 'main'
            '';
          }
        ];

        extraConfig = ''
          set -g allow-passthrough on
          set -g default-shell ${pkgs.fish}/bin/fish
        '';
      };

      firefox = {
        enable = true;
        policies = {
          Preferences = {
            "toolkit.legacyUserProfileCustomizations.stylesheets" = { Value = true; Status = "locked"; };
            "layout.css.devPixelsPerPx" = { Value = "1.0"; Status = "locked"; };
          };
        };
        profiles = {
          default = {
            id = 0;
            name = "default";
            isDefault = true;
            settings = {
              "browser.tabs.inTitlebar" = 0;
              "full-screen-api.ignore-widgets" = true;
              "full-screen-api.exit-on.windowRaise" = false;
              /*
              "extensions.activeThemeId" = with config.nur.repos.rycee;
                firefox-addons.dracula-dark-colorscheme.addonId;
              */
            };
            userChrome = builtins.readFile ./domain/firefox/userChrome.css;
            extensions = with nixospkgs.nur.repos.rycee.firefox-addons; [
              dracula-dark-colorscheme
              ublock-origin
              video-downloadhelper
            ];
          };
        };
      };

      waybar = {
        enable = true;
        style = builtins.readFile ./domain/hypr/waybar.style.css;
        settings = {
          mainBar = {
            layer = "top";
            position = "bottom";
            height = 32;
            width = null;
            margin = "0 0 0 0";
            spacing = 2;
            fixed-center = true;
            modules-left = ["hyprland/workspaces"];
            modules-center = ["cava" "mpd"];
            modules-right = ["backlight" "battery" "pulseaudio" "tray" "clock"];
            "hyprland/workspaces" = {
              format = " {icon} {windows} ";
              window-rewrite-default = "󰘔";
              window-rewrite = {
                "firefox" = "";
                ".slippi-netplay-wrapped" = "";
                "com.obsproject.Studio" = "";
                "code-url-handler" = "󰨞";
                "Gimp-2.10" = "";
                "kitty" = "";
                "wezterm" = "";
                "discord" = "󰙯";
                "thunar" = "";
                "neovide" = "";
                "glrnvim" = "";
                "vlc" = "󰕼";
              };
              on-click = "activate";
              move-to-monitor = true;
              all-outputs = true;
              sort-by-number = true;
              format-icons = {
                "1" = "1";
                "2" = "2";
                "3" = "3";
                "4" = "4";
                "5" = "5";
                "6" = "6";
                "7" = "7";
                "8" = "8";
                "9" = "9";
                "10" = "10";
                "focused" = "";
                "default" = "";
              };
              persistent-workspaces = {
                "*" = [1];
                "HDMI-A-1" = [2];
              };
              on-scroll-up = "hyprctl dispatch workspace e+1";
              on-scroll-down = "hyprctl dispatch workspace e-1";
            };
            "tray" = {
              icon-spacing = 20;
              spacing = 5;
            };
            "clock" = {
              tooltip-format = ''
                <big>{:%A, %d.%B %Y }</big>
                <tt><small>{calendar}</small></tt>
              '';
              format = "{:%a %m/%d %I:%M:%S %p}";
              interval = 1;
            };
            "backlight" = {
              format = "{icon} {percent: >3}%";
              format-icons = [""];
              on-scroll-down = "brightnessctl -c backlight set 1%-";
              on-scroll-up = "brightnessctl -c backlight set +1%";
            };
            "battery" = {
              format = "{icon} {capacity: >3}%";
              format-icons = [ "" "" "" "" "" ];
              states = { warning = 30; critical = 15; };
            };
            "pulseaudio" = {
              format = "{icon} {volume}% {format_source}";
              format-bluetooth = "{volume}% {icon} {format_source}";
              format-bluetooth-muted = " {icon} {format_source}";
              format-muted = " {format_source}";
              format-source = "";
              format-source-muted = "";
              format-icons = {
                headphone = "";
                hands-free = "";
                headset = "";
                phone = "";
                portable = "";
                car = "";
                default = [ "" "" "" ];
              };
              on-click = "pavucontrol-qt";
              on-click-right = "pactl set-source-mute @DEFAULT_SOURCE@ toggle";
              scroll-step = 3;
            };
            "mpd" = {
              format =
                "{title} - {artist}  {stateIcon}  {elapsedTime:%M:%S}/{totalTime:%M:%S}";
              format-disconnected = "";
              format-stopped = "";
              unknown-tag = "N/A";
              interval = 2;
              consume-icons = {
                on = " ";
              };
              random-icons = {
                on = " ";
              };
              repeat-icons = {
                "on" = " ";
              };
              single-icons = {
                "on" = "1 ";
              };
              state-icons = {
                "paused" = "";
                "playing" = "";
              };
              tooltip-format = "mpd (connected)";
              tooltip-format-disconnected = "mpd (disconnected)";
              on-click = "mpc toggle";
              on-click-right = "kitty --class float_kitty sh -c ncmpcpp";
              artist-len = 20;
              title-len = 20;
            };
            "cava" = {
              cava_config = "/home/dz/.config/cava/config";
              hide_on_silence = true;
              bars = 16;
              format-icons = [
                "<span foreground=\"#735cb8\">▁</span>"
                "<span foreground=\"#8a5cb8\">▂</span>"
                "<span foreground=\"#a15cb8\">▃</span>"
                "<span foreground=\"#b85cb8\">▄</span>"
                "<span foreground=\"#b85ca1\">▅</span>"
                "<span foreground=\"#b85c8a\">▆</span>"
                "<span foreground=\"#b85c73\">▇</span>"
                "<span foreground=\"#b85c5c\">█</span>"
              ];
              sleep_timer = 5;
              bar_delimiter = 0;
              framerate = 30;
              monstercat = false;
              waves = false;
              autosens = 0;
              sensitivity = 4;
            };
          };
        };
      };

      wpaperd = {
        enable = true;
        settings = {
          default = {
            path = "/home/dz/Pictures/Wallpapers/default.png";
          };
        };
      };
    };

    wayland.windowManager.hyprland = {
      enable = true;
      package = hyprland.packages.${pkgs.stdenv.hostPlatform.system}.hyprland;
      plugins = with hyprland-plugins.packages.${pkgs.stdenv.hostPlatform.system}; [
        # hyprland-dyncursors.packages.${pkgs.stdenv.hostPlatform.system}.hypr-dynamic-cursors
        # hyprland-hyprfocus.packages.${pkgs.stdenv.hostPlatform.system}.hyprfocus
        # hyprtrails
        # hyprwinwrap
      ];
      settings = {
        monitor = [
          "eDP-1, 1920x1080, 0x0, 1.0"
          "VGA-0, 1920x1080, auto-right, 1.0"
          "HDMI-A-1, 1920x1080, auto-right, 1.0"
        ];
        exec-once = [
          "wpaperd"
          "waybar"
          "nm-applet"
          "blueman-applet"
          "hyprctl setcursor BreezeX-RosePineDawn-Linux 32"
        ];
        env = [
          "XCURSOR_SIZE,32"
          "HYPRCURSOR_SIZE,32"
        ];
        general = {
          gaps_in = 4;
          gaps_out = 12;
          border_size = 2;
          "col.active_border" = "rgba(33ccffee) rgba(ff33ccee) rgba(ccff33ee) 45deg";
          "col.inactive_border" = "rgba(595959aa)";
          resize_on_border = false;
          allow_tearing = false;
          layout = "dwindle";
        };
        decoration = {
          rounding = 10;
          active_opacity = 1.0;
          inactive_opacity = 1.0;
          shadow = {
            enabled = true;
            range = 4;
            render_power = 3;
            color = "rgba(1a1a1aee)";
          };
          blur = {
            enabled = true;
            size = 11;
            passes = 3;
            vibrancy = 0.1696;
            vibrancy_darkness = 0.9696;
          };
        };
        animations = {
          enabled = true;
        };
        dwindle = {
          pseudotile= true;
          preserve_split = true;
        };
        master = {
          new_status = "master";
        };
        misc = {
          force_default_wallpaper = 0;
          disable_hyprland_logo = true;
        };
        input = {
          kb_layout = "us";
          follow_mouse = 1;
          sensitivity = 0;
          touchpad = {
            natural_scroll = true;
          };
        };
        "$mod" = "SUPER";
        bind = [
          "$mod, 36, exec, kitty"
          "$mod, W, exec, firefox"
          "$mod, Q, killactive,"
          "$mod, M, exit,"
          "$mod, E, exec, $fileManager"
          "$mod, V, togglefloating,"
          "$mod, F, fullscreen"
          "$mod, space, exec, dzKeyMenu"
          "$mod, slash, exec, openApp"
          "$mod, P, pseudo, # dwindle"
          "$mod, J, togglesplit, # dwindle"
          "$mod, mouse_down, workspace, e+1"
          "$mod, mouse_up, workspace, e-1"
          ",121, exec, bash -c volumeToggleMute"
        ] ++ (
          builtins.concatLists (
            builtins.genList
              (i:
                let
                  ws = i + 1;
                  k = ws - ((ws / 10) * 10);
                in [
                  "$mod, ${toString k}, workspace, ${toString ws}"
                  "$mod SHIFT, ${toString k}, movetoworkspace, ${toString ws}"
                ]
              )
              10
          )
        ) ++ (
          builtins.concatLists (
            builtins.map
              (d: [
                "$mod, ${d.k}, movefocus, ${d.c}"
                "$mod SHIFT, ${d.k}, movewindow, ${d.c}"
                "$mod CTRL, ${d.k}, resizewindowpixel, ${d.r},activewindow"
                "$mod CTRL SHIFT, ${d.k}, swapwindow, ${d.c}"
              ])
              [
                { c = "u"; k = "up";    r = "0 -10"; }
                { c = "l"; k = "left";  r = "-10 0"; }
                { c = "d"; k = "down";  r = "0 10" ; }
                { c = "r"; k = "right"; r = "10 0" ; }
              ]
          )
        );
        binde = [
          ",122, exec, bash -c volumeDown"
          ",123, exec, bash -c volumeUp"
          ",232, exec, bash -c brightnessDown"
          ",233, exec, bash -c brightnessUp"
        ];
        bindm = [
          "$mod, mouse:272, movewindow"
          "$mod SHIFT, mouse:272, resizewindow"
        ];
        windowrulev2 = [
          "suppressevent maximize, class:.*"
          "float, class:(float_.*)"
          "float, class:(Ztr)"
          "center 1, class:(Ztr)"
          "float, class:(ztr)"
          "center 1, class:(ztr)"
        ];
        layerrule = [
          "blur , waybar"
          "ignorezero , waybar"
          "blur , notifications"
          "ignorezero , notifications"
          "blur , rofi"
          "ignorezero , rofi"
          "noanim , wpaperd.*"
        ];
          /*
        "plugin:hyprtrails" = {
          color = "rgba(bb44cc44)";
        };
        "plugin:hyprwinwrap" = {
          class = "kitty-hww";
        };
        "plugin:hyperfocus" = {
          enabled = true;
          animate_floating = "yes";
          animate_workspacechange = "yes";
          focus_animation = "shrink";
          bezier = ["realsmooth, 0.28,0.29,.69,1.08"];
          shrink = {
            shrink_percentage = 0.8;
            in_bezier = "realsmooth";
            in_speed = 1;
            out_bezier = "realsmooth";
            out_speed = 2;
          };
        };
        "plugin:dynamic-cursors" = {
          enabled = true;
          mode = "tilt";
          shake = {
            enabled = false;
          };
        };
          */
      };
    };

    xsession.windowManager.bspwm = {
      enable = true;
      extraConfigEarly = byHostname.${hostname}.bspwmExtraConfig + ''
        autorandr -c
        xsetroot -cursor_name left_ptr
        xset s off -dpms
        systemctl --user start picom
        systemctl --user start polybar
        systemctl --user start redshift
        systemctl --user start bspwm-polybar
        nitrogen --restore
        blueman-applet &
        nm-applet &
      '';
      extraConfig = ''
        bspwm-reset-monitors.js
      '';
      rules = {
        ztr = {
          border = false;
          focus = false;
          state = "floating";
          center = true;
        };
        Ztr = {
          border = false;
          focus = false;
          state = "floating";
          center = true;
        };
      };
      settings = {
        focus_follows_pointer = true;
        pointer_follows_focus = true;
        pointer_follows_monitor = true;
        border_width = 2;
        normal_border_color  = "#646464";
        active_border_color  = "#645276";
        focused_border_color = "#a487c7";
      };
    };

    services = {
      autorandr.enable = true;
      dunst = {
        enable = true;
        iconTheme.package = pkgs.dracula-icon-theme;
        iconTheme.name = "Dracula";
        settings = {
          global = {
            transparency = 10;
            corner_radius = 13;
            background = "#1E1F29";
          };
        };
      };
      cliphist = { enable = true; };
      sxhkd = {
        enable = true;
        keybindings = {
          "super + shift + q" = "bspc quit";
          "super + q" = "bspc node --close";
          "super + space" = "dzKeyMenu";
          "super + slash" = "openApp";
          "super + Return" = "kitty";
          "super + w" = "firefox";
          "super + e" = "thunar";
          "super + grave" = "bspwm-cycle-monitor-focus.js";
          "super + {t,shift + t,f,m}" = "bspc node -t {tiled,pseudo_tiled,floating,fullscreen}";
          "super + {1-9,0,equal}" = "bspwm-focus-desktop.js {1-9,10,f}";
          "super + shift + {1-9,0,plus}" = "bspwm-move-to-desktop.js -d {1-9,10,f}";
          "super + {Left,Right,Up,Down}" = "bspc node -f {west,east,north,south}";
          "super + ctrl + Left" = "bspc node -z left -10 0 || bspc node -z right -10 0";
          "super + ctrl + Right" = "bspc node -z left 10 0 || bspc node -z right 10 0";
          "super + ctrl + Up" = "bspc node -z top 0 -10 || bspc node -z bottom 0 -10";
          "super + ctrl + Down" = "bspc node -z top 0 10 || bspc node -z bottom 0 10";
          "XF86MonBrightnessUp" = "brightnessUp";
          "XF86MonBrightnessDown" = "brightnessDown";
          "XF86AudioRaiseVolume" = "volumeUp";
          "XF86AudioLowerVolume" = "volumeDown";
          "XF86AudioMute" = "volumeToggleMute";
          "shift + XF86AudioRaiseVolume" = "next.py";
          "shift + XF86AudioLowerVolume" = "prev.py";
          "shift + XF86AudioMute" = "pause.py";
          "XF86AudioPlay" = "pause.py";
          "XF86AudioNext" = "next.py";
          "XF86AudioPrev" = "prev.py";
          "Print" = "ss_dir_scrot";
          "ctrl + Print" = "ss_dir_scrot --select";
          "shift + Print" = "ss_dir_scrot -u";
        };
      };
      polybar =
        let
          polybar_cava = pkgs.writeShellApplication {
            name = "polybar_cava";
            runtimeInputs = [ pkgs.coreutils pkgs.cava pkgs.gnused ];
            text = builtins.readFile ./domain/polybar/cava.sh;
          };
          extraBinPath = lib.makeBinPath [
            pkgs.coreutils
            pkgs.systemd
            pkgs.which
            pkgs.bspwm
            pkgs.nodejs
            pkgs.pamixer
            pkgs.pulseaudio
            polybar_cava
          ];
        in {
          enable = true;
          package = (pkgs.polybar.override {
            alsaSupport = true;
            iwSupport = true;
            githubSupport = true;
            pulseSupport = true;
            mpdSupport = true;
          });
          config = ./domain/polybar/config.ini;
          script = ''
            export PATH=$PATH:/home/dz/Projects/dz-bspwm/bin:${extraBinPath}

            for m in $(polybar --list-monitors | cut -d":" -f1); do
              MONITOR=$m polybar --reload example &
            done
          '';
        };

      redshift = {
        enable = true;
        tray = true;
        latitude = 41.86;
        longitude = -88.12;
      };

      picom = {
        enable = true;
        package = pkgs.picom;
        backend = "glx";
        vSync = true;
        extraArgs = ["--config" "/home/dz/.config/picom/final.conf"];
        settings = {
          shadow = true;
          shadow-radius = 50;
          shadow-opacity = 0.35;
          shadow-offset-x = -49;
          shadow-offset-y = -47;
          shadow-color = "#00020b";
          frame-opacity = 0.95;
          frame-opacity-for-same-colors = true;
          inner-border-width = 1;
          corner-radius = 13;
          blur-method = "dual_kawase";
          blur-strength = 10;
          blur-background = true;
          blur-background-frame = true;
          dithered-present = false;
          detect-client-opacity = true;
          detect-transient = true;
          detect-client-leader = true;
          glx-no-stencil = true;
          glx-no-rebind-pixmap = true;
          use-damage = true;
          xrender-sync-fence = true;
        };
      };
    };

    # The state version is required and should stay at the version you
    # originally installed.
    home.stateVersion = "24.05";
  };

  users.groups.voiders.members = [ "dz" "mopidy" ];
  systemd.user.services.picom.wantedBy = [];
  systemd.user.services.polybar.wantedBy = [];
  systemd.user.services.redshift.wantedBy = [];
  systemd.user.services.bspwm-polybar = {
    enable = true;
    description = "control dzbspwm polybar module";
    serviceConfig = {
      Type = "exec";
      ExecStart = "/home/dz/Projects/dz-bin/bspwm-polybar-watch";
      Restart = "on-failure";
      Environment="PATH=$PATH:${lib.makeBinPath [ pkgs.coreutils pkgs.bash pkgs.which pkgs.ps pkgs.nodejs pkgs.bspwm pkgs.polybar ]}:/home/dz/Projects/dz-bin:/home/dz/Projects/dz-bspwm/bin";
    };
    wantedBy = [];
  };
  programs.dconf.enable = true;

  # Allow unfree packages
  nixpkgs.config.allowUnfree = true;
  nixpkgs.config.nvidia.acceptLicense = true;


  ## environment.sessionVariables.NIXOS_OZONE_WL = "1";
  environment.pathsToLink = [ "/share/zsh" ]; # idk why I did this..?

  # List packages installed in system profile. To search, run:
  # $ nix search wget
  environment.systemPackages = with pkgs; [
    alsa-utils
    any-nix-shell
    ascii-image-converter
    bat
    blesh
    bc
    bibata-cursors
    catppuccin-cursors.mochaRosewater
    rose-pine-cursor
    brightnessctl
    cargo
    cava
    cavalier
    clojure
    clojure-lsp
    clj-kondo
    joker
    coreutils
    discord
    dmenu
    emacs
    esh
    fastfetch
    fd
    file
    ffmpeg
    fzf
    gcc
    gh
    github-desktop
    gjs
    glrnvim
    gnum4
    gnused
    grc
    grim
    gtk-server
    gtk3
    gtk3-x11
    gtk4
    haskell-language-server
    heroku
    htop
    jq
    jp2a
    killall
    kitty
    libnotify
    mpc-cli
    ncmpcpp
    networkmanagerapplet
    nitrogen
    nix-prefetch-github
    nodejs
    nwjs-sdk
    pamixer
    pavucontrol
    lxqt.pavucontrol-qt
    pcmanfm
    perl
    pkg-config
    pulseaudio
    (python3.withPackages (python-pkgs: [
      python-pkgs.beautifulsoup4
      python-pkgs.coconut
      python-pkgs.dmenu-python
      python-pkgs.mpd2
      python-pkgs.requests
      python-pkgs.xlib
      python-pkgs.pip
    ]))
    qimgv
    ripgrep
    rofi-wayland
    rust-analyzer
    scrot
    ### Need the thing
    sddm-chili-theme
    ####
    sddm-dz.packages.${pkgs.hostPlatform.system}.sddm-dz
    slurp
    ssbm.packages.${pkgs.hostPlatform.system}.slippi-launcher
    ssbm.packages.${pkgs.hostPlatform.system}.slippi-netplay
    ssbm.packages.${pkgs.hostPlatform.system}.slippi-playback
    stack
    traceroute
    transmission_4-qt
    ttyd
    typescript
    typescript-language-server
    ueberzugpp
    unzip
    vim
    vlc
    vscode
    waybar
    waybox
    wev
    wget
    wl-clipboard
    wmctrl
    wmutils-core
    wofi
    wpaperd
    (pkgs.wrapOBS {
      plugins = with pkgs.obs-studio-plugins; [
        obs-pipewire-audio-capture
        obs-vkcapture
        wlrobs
      ];
    })
    xclip
    xdo
    xdotool
    (pkgs.stdenv.mkDerivation {
      pname = "xmoctrl";
      version = "0.0.1";

      src = lib.cleanSourceWith {
        filter = name: type: false;
        src = lib.cleanSource ./.;
      };

      buildInputs = [
        pkgs.xmonadctl
        (pkgs.haskellPackages.ghcWithPackages (
          haskellPackages: [(mk_xmolib haskellPackages)]
        ))
      ];
      propagatedBuildInputs = [ pkgs.xmonadctl ];

      dontConfigure = true;
      buildPhase = ''
        echo -e \
          "\nimport qualified Xmolib.Entry.Xmoctrl as Xmolib"\
          "\nmain :: IO ()"\
          "\nmain = Xmolib.runXmoctrl" > xmoctrl.hs
        ghc xmoctrl.hs
      '';
      installPhase = ''
        mkdir -p $out/bin
        cp xmoctrl $out/bin/xmoctrl
      '';

      meta = {
        description = "command runner built on top of xmonadctl";
        homepage = "https://github.com/mitchdzugan/dz-nixos";
        license = lib.licenses.mit;
        maintainers = with lib.maintainers; [ mitchdzugan ];
        platforms = lib.platforms.linux;
      };
    })
    xmonadctl
    xpra
    xorg.xev
    yarn
  ];

  # Some programs need SUID wrappers, can be configured further or are
  # started in user sessions.
  # programs.mtr.enable = true;
  # programs.gnupg.agent = {
  #   enable = true;
  #   enableSSHSupport = true;
  # };

  # List services that you want to enable:

  # Enable the OpenSSH daemon.
  services.openssh.enable = true;

  # Open ports in the firewall.
  # networking.firewall.allowedTCPPorts = [ ... ];
  # networking.firewall.allowedUDPPorts = [ ... ];
  # Or disable the firewall altogether.
  # networking.firewall.enable = false;

  hardware.system76.enableAll = hostname == "mitch-desktop";

  hardware.graphics.enable = true;

  services.xserver.videoDrivers = byHostname.${hostname}.videoDrivers;

  hardware.nvidia = byHostname.${hostname}.nvidia;

  programs.gamemode.enable = true;

  programs.xonsh = { enable = true; package = dz_xonsh; };

  programs.steam = {
    enable = true;
    remotePlay.openFirewall = true;
    dedicatedServer.openFirewall = true;
    localNetworkGameTransfers.openFirewall = true;
  };

  programs.nix-ld.enable = true;

  programs.thunar = {
    enable = true;
    plugins = with pkgs.xfce; [
      thunar-archive-plugin
      thunar-media-tags-plugin
      thunar-volman
    ];
  };
  programs.xfconf.enable = true;
  programs.file-roller.enable = true;
  services.gvfs.enable = true;
  services.tumbler.enable = true;

  services.mopidy = {
    enable = true;
    extensionPackages = [ pkgs.mopidy-iris pkgs.mopidy-local pkgs.mopidy-mpd ];
    configuration = ''
      [local]
      media_dir = ${musicDir}

      [m3u]
      playlists_dir = ${musicDir}/Playlists

      [audio]
      output = pulsesink server=127.0.0.1
    '';
  };

  /*
  services.emacs = {
    enable = true;
    package = pkgs.emacs;
  };
  */

  services.autorandr = {
    enable = true;
    profiles = autorandr_profiles;
  };

  services.avahi = {
    nssmdns4 = true;
    enable = true;
    ipv4 = true;
    ipv6 = true;
    publish = {
      enable = true;
      addresses = true;
      workstation = true;
    };
  };

  fonts.packages = with pkgs; [
    dina-font
    fira-code
    fira-code-symbols
    font-awesome
    liberation_ttf
    monaspace
    mplus-outline-fonts.githubRelease
    nerd-fonts.comic-shanns-mono
    nerd-fonts.daddy-time-mono
    nerd-fonts.go-mono
    nerd-fonts.heavy-data
    nerd-fonts.monaspace
    nerd-fonts.open-dyslexic
    nerd-fonts.proggy-clean-tt
    nerd-fonts.recursive-mono
    nerd-fonts.sauce-code-pro
    nerd-fonts.space-mono
    nerd-fonts.symbols-only
    nerd-fonts.terminess-ttf
    nerd-fonts.ubuntu-sans
    noto-fonts-cjk-sans
    noto-fonts-emoji
    powerline-fonts
    powerline-symbols
    proggyfonts
    ubuntu_font_family
  ];
  fonts.enableDefaultPackages = true;
  fonts.fontconfig = {
    defaultFonts = {
      serif = [  "Liberation Serif" ];
      sansSerif = [ "Ubuntu" ];
      monospace = [ "MonaspiceKr Nerd Font Mono" ];
    };
  };

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "24.05"; # Did you read the comment?

}
