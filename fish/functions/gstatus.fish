function gstatus --description 'git status'
    echo Checking Dotfiles
    echo --------------------
    cd /home/merrinx/Sources/Dotfiles/ $argv
    git status $argv
    echo --------------------
    echo Checking Secrets
    echo --------------------
    cd /home/merrinx/Sources/Secrets/ $argv
    git status $argv
    echo --------------------
    echo Checking Documents
    echo --------------------
    cd /home/merrinx/Documents/ $argv
    git status $argv
    echo --------------------
    echo Checking Suckless
    echo --------------------
    cd /home/merrinx/Projects/Suckless/ $argv
    git status $argv
    echo --------------------
    echo Checking Courses
    echo --------------------
    cd /home/merrinx/Projects/Courses/ $argv
    git status $argv
    echo --------------------
    echo Checking Algorithms
    echo --------------------
    cd /home/merrinx/Projects/Algorithms/ $argv
    git status $argv
    echo --------------------
    echo Checking Masterclass
    echo --------------------
    cd /home/merrinx/Projects/Masterclass/ $argv
    git status $argv
    echo --------------------
    echo Checking Latest project
    echo --------------------
    cd /home/merrinx/Projects/assignment-3-cache-johnnycach/ $argv
    git status $argv
    echo --------------------
    echo COMPLETE!
    echo --------------------
    cd /home/merrinx/ $argv
end
