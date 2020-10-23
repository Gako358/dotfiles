function gs --description 'git status'
    echo (set_color brred) Checking Dotfiles (set_color normal)
    echo (set_color brgreen) -------------------- (set_color normal)
    cd /home/merrinx/Sources/Dotfiles/ $argv
    git status $argv
    echo (set_color brgreen) -------------------- (set_color normal)
    echo (set_color brred) Checking Secrets (set_color normal)
    echo (set_color brgreen) -------------------- (set_color normal)
    cd /home/merrinx/Sources/Secrets/ $argv
    git status $argv
    echo (set_color brgreen) -------------------- (set_color normal)
    echo (set_color brred) Checking Documents (set_color normal)
    echo (set_color brgreen) -------------------- (set_color normal)
    cd /home/merrinx/Documents/ $argv
    git status $argv
    echo (set_color brgreen) -------------------- (set_color normal)
    echo (set_color brred) Checking Suckless (set_color normal)
    echo (set_color brgreen) -------------------- (set_color normal)
    cd /home/merrinx/Projects/Suckless/ $argv
    git status $argv
    echo (set_color brgreen) -------------------- (set_color normal)
    echo (set_color brred) Checking Courses (set_color normal)
    echo (set_color brgreen) -------------------- (set_color normal)
    cd /home/merrinx/Projects/Courses/ $argv
    git status $argv
    echo (set_color brgreen) -------------------- (set_color normal)
    echo (set_color brred) Checking Algorithms (set_color normal)
    echo (set_color brgreen) -------------------- (set_color normal)
    cd /home/merrinx/Projects/Algorithms/ $argv
    git status $argv
    echo (set_color brgreen) -------------------- (set_color normal)
    echo (set_color brred) Checking Masterclass (set_color normal)
    echo (set_color brgreen) -------------------- (set_color normal)
    cd /home/merrinx/Projects/Masterclass/ $argv
    git status $argv
    echo (set_color brgreen) -------------------- (set_color normal)
    echo (set_color brred) Checking Latest Project (set_color normal)
    echo (set_color brgreen) -------------------- (set_color normal)
    cd /home/merrinx/Projects/assignment-3-cache-johnnycach/ $argv
    git status $argv
    echo (set_color brgreen) ----------COMPLETE!---------- (set_color normal)
    cd /home/merrinx/ $argv
end
