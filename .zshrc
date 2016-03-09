# Source Prezto.
if [[ -s "${ZDOTDIR:-$HOME}/.zprezto/init.zsh" ]]; then
  source "${ZDOTDIR:-$HOME}/.zprezto/init.zsh"
fi

# customize sorin theme a bit
BASIC_PROMPT='λ %F{blue}%~/%F{white}'
PROMPT='$BASIC_PROMPT '

function prompt_sorin_git_info {
  if (( _prompt_sorin_precmd_async_pid > 0 )); then
    # Append Git status.
    if [[ -s "$_prompt_sorin_precmd_async_data" ]]; then
      alias typeset='typeset -g'
      source "$_prompt_sorin_precmd_async_data"
      PROMPT='$BASIC_PROMPT${git_info:+${(e)git_info[status]}} '
      unalias typeset
    fi

    # Reset PID.
    _prompt_sorin_precmd_async_pid=0

    # Redisplay prompt.
    zle && zle reset-prompt
  fi
}

# Local config
[[ -f ~/.zshrc.local ]] && source ~/.zshrc.local
