#!/usr/bin/env fish

alias ls "grc ls --color=always"
function fish_greeting
  fastfetch \
    --separator-output-color black \
    --logo-width 37 \
    --logo-height 17 \
    --logo-padding-left 1 \
    --logo-padding-top 3 \
    --logo-padding-right 3 \
    --logo-type kitty-direct \
    --logo ~/.config/fastfetch/logo.nix.2.png
end

function firsty
  if [ $(count $argv) -eq 0 ]
    return 1
  end
  set -l curr $argv[1]
  set -l rest $argv[2..]
  if [ -z $curr ]
    firsty $rest
  else if [ $curr -eq 0 ]
    firsty $rest
  else
    echo $curr
  end
end

function nixroot
  set -l curr $(path resolve $argv[1])
  set -l next $(path resolve "$curr/..")
  if [ -f "$curr/flake.nix" ]
    echo flake $curr
  else if [ -f "$curr/shell.nix" ]
    echo pkg $curr
  else if [ -f "$curr/default.nix" ]
    echo pkg $curr
  else if [ $curr != $next ]
    nixroot $next
  else
    echo "err@{$(pwd) is not in nix dir}" >&2
    return 1
  end
end

function zslurp
  set -l varname $argv[1]
  read -z lines
  set $varname (string split \n -- $lines)
end

function _wrap_zssh_arg
  function wrap_host
    echo "mitch-$argv[1].local"
  end
  set -l regex "^(([a-zA-Z][a-zA-Z0-9]+)(@))?(hp|gazelle|desktop)(.*)"
  set -g matches
  string match -r $regex $argv[1] | zslurp matches
  switch (count $matches)
    case 7
      echo "$matches[3]@$(wrap_host $matches[5])$matches[6]"
    case 4
      echo "dz@$(wrap_host $matches[2])$matches[3]"
    case '*'
      return 1
  end
end

function _zssh_cmd
  set -l cmd $argv[1]
  set -l final
  for arg in $argv[2..]
    set final $final $(_wrap_zssh_arg $arg)
  end
  if [ $cmd = "ssh" ]
    set final $final -t fish
  end
  echo $cmd $final
  $cmd $final
end

alias zssh="_zssh_cmd ssh"
alias zscp="_zssh_cmd scp"

function configure_my_tide
  set tide_left_prompt_items vi_mode\x1epwd\x1echaracter
  set tide_right_prompt_items \
    status\x1ecmd_duration\x1ejobs\x1egit\x1edirenv\x1enode\x1epython\x1enix_shell\x1econtext
  set tide_aws_bg_color 1C1C1C
  set tide_aws_color FF9900
  set tide_aws_icon \uf270
  set tide_character_color 5FD700
  set tide_character_color_failure FF0000
  set tide_character_icon \u276f
  set tide_character_vi_icon_default \u276e
  set tide_character_vi_icon_replace \u25b6
  set tide_character_vi_icon_visual V
  set tide_cmd_duration_bg_color 1C1C1C
  set tide_cmd_duration_color 87875F
  set tide_cmd_duration_decimals 0
  set tide_cmd_duration_icon \uf252
  set tide_cmd_duration_threshold 3000
  set tide_context_always_display true
  set tide_context_bg_color 111111
  set tide_context_color_default D7AF87
  set tide_context_color_root D7AF00
  set tide_context_color_ssh D7AF87
  set tide_context_hostname_parts 1
  set tide_crystal_bg_color 1C1C1C
  set tide_crystal_color FFFFFF
  set tide_crystal_icon \ue62f
  set tide_direnv_bg_color 1C1C1C
  set tide_direnv_bg_color_denied 1C1C1C
  set tide_direnv_color D7AF00
  set tide_direnv_color_denied FF0000
  set tide_direnv_icon \u25bc
  set tide_distrobox_bg_color 1C1C1C
  set tide_distrobox_color FF00FF
  set tide_distrobox_icon \U000f01a7
  set tide_docker_bg_color 1C1C1C
  set tide_docker_color 2496ED
  set tide_docker_default_contexts default\x1ecolima
  set tide_docker_icon \uf308
  set tide_elixir_bg_color 1C1C1C
  set tide_elixir_color 4E2A8E
  set tide_elixir_icon \ue62d
  set tide_gcloud_bg_color 1C1C1C
  set tide_gcloud_color 4285F4
  set tide_gcloud_icon \U000f02ad
  set tide_git_bg_color 1C1C1C
  set tide_git_bg_color_unstable 1C1C1C
  set tide_git_bg_color_urgent 1C1C1C
  set tide_git_color_branch 5FD700
  set tide_git_color_conflicted FF0000
  set tide_git_color_dirty D7AF00
  set tide_git_color_operation FF0000
  set tide_git_color_staged D7AF00
  set tide_git_color_stash 5FD700
  set tide_git_color_untracked 00AFFF
  set tide_git_color_upstream 5FD700
  set tide_git_icon \uf1d3
  set tide_git_truncation_length 24
  set tide_git_truncation_strategy \x1d
  set tide_go_bg_color 1C1C1C
  set tide_go_color 00ACD7
  set tide_go_icon \ue627
  set tide_java_bg_color 1C1C1C
  set tide_java_color ED8B00
  set tide_java_icon \ue256
  set tide_jobs_bg_color 1C1C1C
  set tide_jobs_color 5FAF00
  set tide_jobs_icon \uf013
  set tide_jobs_number_threshold 1000
  set tide_kubectl_bg_color 1C1C1C
  set tide_kubectl_color 326CE5
  set tide_kubectl_icon \U000f10fe
  set tide_left_prompt_frame_enabled false
  set tide_left_prompt_prefix
  set tide_left_prompt_separator_diff_color \ue0b0
  set tide_left_prompt_separator_same_color \ue0b5
  set tide_left_prompt_suffix \ue0b0
  set tide_nix_shell_bg_color 1C1C1C
  set tide_nix_shell_color 7EBAE4
  set tide_nix_shell_icon \uf313
  set tide_node_bg_color 1C1C1C
  set tide_node_color 44883E
  set tide_node_icon \ue24f
  set tide_os_bg_color 1C1C1C
  set tide_os_color EEEEEE
  set tide_os_icon \uf313
  set tide_php_bg_color 1C1C1C
  set tide_php_color 617CBE
  set tide_php_icon \ue608
  set tide_private_mode_bg_color 1C1C1C
  set tide_private_mode_color FFFFFF
  set tide_private_mode_icon \U000f05f9
  set tide_prompt_add_newline_before false
  set tide_prompt_color_frame_and_connection 6C6C6C
  set tide_prompt_color_separator_same_color 949494
  set tide_prompt_icon_connection \x20
  set tide_prompt_min_cols 34
  set tide_prompt_pad_items true
  set tide_prompt_transient_enabled false
  set tide_pulumi_bg_color 1C1C1C
  set tide_pulumi_color F7BF2A
  set tide_pulumi_icon \uf1b2
  set tide_pwd_bg_color 1D111F
  set tide_pwd_color_anchors 00AFFF
  set tide_pwd_color_dirs 0087AF
  set tide_pwd_color_truncated_dirs 8787AF
  set tide_pwd_icon \uf07c
  set tide_pwd_icon_home \uf015
  set tide_pwd_icon_unwritable \uf023
  set tide_pwd_markers \x2ebzr\x1e\x2ecitc\x1e\x2egit\x1e\x2ehg\x1e\x2enode\x2dversion\x1e\x2epython\x2dversion\x1e\x2eruby\x2dversion\x1e\x2eshorten_folder_marker\x1e\x2esvn\x1e\x2eterraform\x1eCargo\x2etoml\x1ecomposer\x2ejson\x1eCVS\x1ego\x2emod\x1epackage\x2ejson\x1ebuild\x2ezig
  set tide_python_bg_color 1C1C1C
  set tide_python_color 00AFAF
  set tide_python_icon \U000f0320
  set tide_right_prompt_frame_enabled false
  set tide_right_prompt_prefix \ue0b2
  set tide_right_prompt_separator_diff_color \ue0b2
  set tide_right_prompt_separator_same_color \ue0b7
  set tide_right_prompt_suffix
  set tide_ruby_bg_color 1C1C1C
  set tide_ruby_color B31209
  set tide_ruby_icon \ue23e
  set tide_rustc_bg_color 1C1C1C
  set tide_rustc_color F74C00
  set tide_rustc_icon \ue7a8
  set tide_shlvl_bg_color 1C1C1C
  set tide_shlvl_color d78700
  set tide_shlvl_icon \uf120
  set tide_shlvl_threshold 1
  set tide_status_bg_color 1C1C1C
  set tide_status_bg_color_failure 2C1C1C
  set tide_status_color 5FAF00
  set tide_status_color_failure D70000
  set tide_status_icon \u2714
  set tide_status_icon_failure \u2718
  set tide_terraform_bg_color 1C1C1C
  set tide_terraform_color 844FBA
  set tide_terraform_icon \U000f1062
  set tide_time_bg_color 1C1C1C
  set tide_time_color 5F8787
  set tide_time_format \x25T
  set tide_toolbox_bg_color 1C1C1C
  set tide_toolbox_color 613583
  set tide_toolbox_icon \ue24f
  set tide_vi_mode_bg_color_default 1C1C1C
  set tide_vi_mode_bg_color_insert 1C1C1C
  set tide_vi_mode_bg_color_replace 1C1C1C
  set tide_vi_mode_bg_color_visual 1C1C1C
  set tide_vi_mode_color_default 949494
  set tide_vi_mode_color_insert 87AFAF
  set tide_vi_mode_color_replace 87AF87
  set tide_vi_mode_color_visual FF8700
  set tide_vi_mode_icon_default D
  set tide_vi_mode_icon_insert I
  set tide_vi_mode_icon_replace R
  set tide_vi_mode_icon_visual V
  set tide_zig_bg_color 1C1C1C
  set tide_zig_color F7A41D
  set tide_zig_icon \ue6a9
end

configure_my_tide
