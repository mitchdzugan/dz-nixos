# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, lib, hostname, home-manager, hyprland, ssbm, sddm-dz, ... }:

let
  lg_hdmi_fingerprint = "00ffffffffffff001e6d6e77f48c0400041f010380462778ea8cb5af4f43ab260e5054210800d1c06140010101010101010101010101e9e800a0a0a0535030203500b9882100001a000000fd0030901ee63c000a202020202020000000fc004c4720554c545241474541520a000000ff003130344e544a4a38533232380a01b8020349f1230907074d100403011f13123f5d5e5f60616d030c002000b83c20006001020367d85dc401788003e30f00186d1a00000205309000045a445a44e305c000e60605015a5a446fc200a0a0a0555030203500b9882100001a5aa000a0a0a0465030203a00b9882100001a565e00a0a0a0295030203500b9882100001aed";
  acer_vga_fingerprint = "00ffffffffffff0004726f04c33a1060011a010368351e78ee0565a756529c270f5054b30c00714f818081c081009500b300d1c00101023a801871382d40582c45000f282100001e000000fd00384b1f4b12000a202020202020000000fc005232343048590a202020202020000000ff005434424141303031323430300a003a";
  acer_hdmi_fingerprint = "00ffffffffffff0004726f04c33a1060011a010380351e78ee0565a756529c270f5054b30c00714f818081c081009500b300d1c00101023a801871382d40582c45000f282100001e000000fd00384b1f4b12000a202020202020000000fc005232343048590a202020202020000000ff005434424141303031323430300a012102031cf1499001030412131f0514230907078301000065030c001000023a801871382d40582c45000f282100001e011d007251d01e206e2855000f282100001e8c0ad08a20e02d10103e96000f2821000018d60980a020e02d10086022000f28210808180000000000000000000000000000000000000000000000000000002e";
  gazelle_fingerprint = "00ffffffffffff0030e44d0700000000001f0104a52213780371c5985e5b8f271b5054000000010101010101010101010101010101015f8780a07038a0463020350058c21000001a000000fd003c90a7a723010a202020202020000000fe004c4720444953504c41590a2020000000fe004c5031353657464a2d53504234014270137900000301145e8700847f079f002f801f0037044c000200040000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000007390";
  autorandr_profiles = {
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
      nvidia = {
        modesetting.enable = true;
        powerManagement.finegrained = false;
        open = false;
        nvidiaSettings = true;
        package = config.boot.kernelPackages.nvidiaPackages.legacy_470;
      };
    };
    mitch-gazelle = {
      musicDir = "/Music";
      kernalPackages = pkgs.linuxPackages_xanmod_latest;
      bspwmExtraConfig = ''
xinputSetTouchpadNaturalScroll
      '';
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
  # services.displayManager.ly.enable = true;
  services.displayManager.sddm.enable = true;
  services.displayManager.sddm.settings = {
    Theme = {
      CursorSize = 24;
      CursorTheme = "Bibata-Modern-Ice";
    };
  };
  services.xserver.displayManager.setupCommands = "${pkgs.autorandr}/bin/autorandr -c";
  services.displayManager.sddm.theme = "sddm-dz";
  # services.displayManager.sddm.theme = "Elegant";
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
  services.xserver.windowManager.bspwm.enable = true;
  # services.desktopManager.plasma6.enable = true;
  # services.xserver.desktopManager.plasma5.enable = true;
  # services.xserver.desktopManager.xfce.enable = true;
  # services.xserver.desktopManager.cinnamon.enable = true;
  # services.xserver.displayManager.gdm.enable = true;
  services.xserver.desktopManager.gnome.enable = true;
  # services.xserver.displayManager.lightdm.enable = true;
  # services.xserver.windowManager.qtile.enable = true;
  # programs.river.enable = true;
  # programs.sway = {
    # enable = true;
    # wrapperFeatures.gtk = true;
  # };
  # programs.hyprland = {
    # enable = true;
    # package = hyprland.packages.${pkgs.stdenv.hostPlatform.system}.hyprland;
    # portalPackage = hyprland.packages.${pkgs.stdenv.hostPlatform.system}.xdg-desktop-portal-hyprland;
  # };
  # services.xserver.desktopManager.pantheon.enable = true;
  # services.xserver.displayManager.lightdm.greeters.pantheon.enable = false;
  # services.xserver.displayManager.lightdm.enable = false;
  # services.pantheon.apps.enable = false;

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
  };

  home-manager.users.dz = { pkgs, ... }: {
    home = {
      pointerCursor = {
        gtk.enable = true;
        x11.enable = true;
        package = pkgs.bibata-cursors;
        name = "Bibata-Modern-Ice";
        size = 24;
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

    programs = {
      autorandr = {
        enable = true;
        profiles = autorandr_profiles;
      };

      bash = {
        enable = true;
        enableCompletion = true;
        bashrcExtra = builtins.readFile ./bashrcExtra;
      };

      git = {
        enable = true;
        userName  = "Mitch Dzugan";
        userEmail = "mitchdzugan@gmail.com";
        extraConfig = {
          init.defaultBranch = "main";
        };
      };

      direnv = {
        enable = true;
        enableBashIntegration = true;
        nix-direnv.enable = true;
      };

      neovim = {
        enable = true;
        defaultEditor = true;
        viAlias = true;
        vimAlias = true;
        vimdiffAlias = true;
        coc = {
          enable = true;
        };
        extraConfig = builtins.readFile ./nvim/init.vim;
        extraLuaConfig = builtins.readFile ./nvim/init.lua;
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
        in with pkgs.vimPlugins; [
          dracula-nvim
          image-nvim
          nui-nvim
          nvim-lspconfig
          nvim-tree-lua
          nvim-treesitter-with-plugins
          nvim-web-devicons
          plenary-nvim
          rainbow-delimiters-nvim
          telescope-nvim
          venn-nvim
        ];
      };

      neovide = {
        enable = true;
        settings = {
          font = {
            normal = ["MonaspiceKr Nerd Font Mono"];
            size = 10.0;
          };
        };
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
            };
            userChrome = builtins.readFile ./userChrome.css;
            extensions = with config.nur.repos.rycee.firefox-addons; [
              ublock-origin
              video-downloadhelper
            ];
          };
        };
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
          "XF86MonBrightnessUp" = "brightnessUp";
          "XF86MonBrightnessDown" = "brightnessDown";
          "XF86AudioRaiseVolume" = "volumeUp";
          "XF86AudioLowerVolume" = "volumeDown";
          "XF86AudioMute" = "volumeToggleMute";
          "XF86AudioPlay" = "pause.py";
          "XF86AudioNext" = "next.py";
          "XF86AudioPrev" = "prev.py";
          "Print" = "ss_dir_scrot";
          "ctrl + Print" = "ss_dir_scrot --select";
          "shift + Print" = "ss_dir_scrot -u";
        };
      };

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

      polybar = 
        let
          polybar_cava = pkgs.writeShellApplication {
            name = "polybar_cava";
            runtimeInputs = [ pkgs.coreutils pkgs.cava pkgs.gnused ];
            text = builtins.readFile ./polybar/cava.sh;
          };
        in {
          enable = true;
          package = (pkgs.polybar.override {
            alsaSupport = true;
            iwSupport = true;
            githubSupport = true;
            pulseSupport = true;
            mpdSupport = true;
          });
          config = ./polybar/config.ini;
          script = ''
export PATH=$PATH:/home/dz/Projects/dz-bspwm/bin:${lib.makeBinPath [ pkgs.coreutils pkgs.systemd pkgs.which pkgs.bspwm pkgs.nodejs pkgs.pamixer pkgs.pulseaudio polybar_cava ]}

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

  # List packages installed in system profile. To search, run:
  # $ nix search wget
  environment.systemPackages = with pkgs; [
    alsa-utils
    bc
    bibata-cursors
    brightnessctl
    cargo
    cava
    cavalier
    coreutils
    discord
    esh
    file
    ffmpeg
    gcc
    gh
    gjs
    gnused
    grim
    gtk3
    gtk3-x11
    gtk4
    heroku
    htop
    jq
    killall
    kitty
    libnotify
    mpc-cli
    ncmpcpp
    networkmanagerapplet
    nitrogen
    nodejs
    nwjs-sdk
    pamixer
    pavucontrol
    pcmanfm
    perl
    picom
    # picom-pijulius
    pkg-config
    pulseaudio
    (python3.withPackages (python-pkgs: [
      python-pkgs.beautifulsoup4
      python-pkgs.dmenu-python
      python-pkgs.mpd2
      python-pkgs.requests
      python-pkgs.xlib
    ]))
    rofi
    scrot
    ### Need the thing
    sddm-chili-theme
    ####
    sddm-dz.packages.${pkgs.hostPlatform.system}.sddm-dz
    slurp
    ssbm.packages.x86_64-linux.slippi-launcher
    ssbm.packages.x86_64-linux.slippi-netplay
    ssbm.packages.x86_64-linux.slippi-playback
    transmission_4-qt
    ttyd
    unzip
    vim
    vlc
    volnoti
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
    xdo
    xdotool
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

  services.xserver.videoDrivers = ["nvidia"];

  hardware.nvidia = byHostname.${hostname}.nvidia;

  programs.gamemode.enable = true;

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

  services.emacs = {
    enable = true;
    package = pkgs.emacs;
  };

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
    mplus-outline-fonts.githubRelease
    nerdfonts
    noto-fonts
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

/*
  xdg.portal = {
    enable = true;
    extraPortals = [
      pkgs.xdg-desktop-portal-kde
      pkgs.xdg-desktop-portal-xapp
    ];
  };
*/

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "24.05"; # Did you read the comment?

}
