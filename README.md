How to install and setup Laptop

BTRFS:

btrfs sub cr /mn/@

btrfs sub cr /mnt/log

btrfs sub cr /mnt/pkg

btrfs sub cr /mnt/snapshots

mount -o relatime,space_cache=v2,ssd,compress=lzo,subvol=@ /dev/sdaN /mnt
mount -o relatime,space_cache=v2,ssd,compress=lzo,subvol=@log /dev/sdaN /mnt/var/log
mount -o relatime,space_cache=v2,ssd,compress=lzo,subvol=@pkg /dev/sdaN /mnt/var/cache/pacman/pkg
mount -o relatime,space_cache=v2,ssd,compress=lzo,subvolid=5 /dev/sdaN /mnt/btrfs


PACKAGES INSTALL:

acpilight

cronie

cmatrix

dmenu2

yay

firefox

thunderbird

gimp

lightdm

lightdm-webkit-theme-aether

lyx

neofetch

zsh

alsa-utils

pavucontrol

pulseaudio

picom

qtile

ranger

slock

ttf-font-awesome

unzip

urxvt-font-size-git

urxvt-perls

rxvt-unicode

vim

xorg-server

ytop

python-cairocffi

python-pip

python-psutil

python-xcffib

BTRFS TOOLS:
snapper

sudo snapper config new 

Edit and set up grub snaps and setup the timers, also enable cronie for timeline snaps

btrfs-grub

snap pac

snap-pac-grub




