# Enable Powerlevel10k instant prompt. Should stay close to the top of ~/.zshrc.
# Initialization code that may require console input (password prompts, [y/n]
# confirmations, etc.) must go above this block; everything else may go below.
if [[ -r "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh" ]]; then
  source "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh"
fi

export ZSH="/home/merrinx/.oh-my-zsh"

ZSH_THEME="powerlevel10k/powerlevel10k"

plugins=(
    git
    zsh-syntax-highlighting
    zsh-autosuggestions
    virtualenv
)

source $ZSH/oh-my-zsh.sh

# Example aliases
# alias zshconfig="mate ~/.zshrc"
# alias ohmyzsh="mate ~/.oh-my-zsh"
alias 11="~/Projects/INF1101/"
alias p11="~/Projects/INF1101/Papers/"
alias 14="~/Projects/INF1400/"
alias p14="~/Projects/INF1400/Papers/"
alias t="~/Projects/Tutorials/"
alias t="~/Projects/Tutorials/"
alias t11="~/Projects/Tutorials/Clang/"
alias t14="~/Projects/Tutorials/Python/"
alias dot="~/Sources/Dotfiles/"
alias doc="~/Documents/"
alias nm="ncmpcpp"

# To customize prompt, run `p10k configure` or edit ~/.p10k.zsh.
[[ ! -f ~/.p10k.zsh ]] || source ~/.p10k.zsh
