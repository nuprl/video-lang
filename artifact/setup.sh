#!/usr/bin/env bash

# Make sure there is a Desktop
mkdir -p ~/Desktop

# Installing vagrant keys
mkdir ~/.ssh
chmod 700 ~/.ssh
cd ~/.ssh
wget --no-check-certificate 'https://raw.github.com/mitchellh/vagrant/master/keys/vagrant.pub' -O authorized_keys
chmod 600 ~/.ssh/authorized_keys
chown -R artifact ~/.ssh

cd ~/Desktop

# Install Racket
# first download Racket v6.9
wget http://mirror.racket-lang.org/installers/6.9/racket-6.9-x86_64-linux.sh

# Do a local install. A unix-style install is preferable in some ways, but the
# permissions are a pain when overriding packages
sh racket-6.9-x86_64-linux.sh --in-place --dest ~/racket

# Add racket to the path
export PATH=~/racket/bin:$PATH
echo "export PATH=~/racket/bin:$PATH" >> ~/.bashrc

# Set up the artifact files
cd /home/artifact
tar -xf icfp-2017-artifact.tar
tar -xf video.tar
tar -xf typed-video.tar
tar -xf paper-src.tar
rm icfp-2017-artifact.tar
rm video.tar
rm typed-video.tar
rm paper-src.tar
mv video ~/Desktop
mv typed-video ~/Desktop
mv paper-src/artifact .
rm -r paper-src/
mv super8-draft.pdf ~/Desktop

# Install Video
cd /home/artifact/Desktop/video
raco pkg install --deps search-auto

# Add Examples folder
cd /home/artifact/Desktop
cp -r /home/artifact/Desktop/video/video/examples .

# Build the paper (never mind)
# cd /home/artifact/Desktop/paper-src
# make

# Place paper on Desktop
# cd /home/artifact/Desktop
# cp /home/artifact/Desktop/paper-src/paper.pdf super8.pdf

# Create the README
cd /home/artifact/artifact
scribble +m --htmls README.scrbl
ln -s /home/artifact/artifact/README/index.html ~/Desktop/README.html

# Add User Manual
cd ~/Desktop
ln -s /home/artifact/video/video/doc/video/index.html manual.html

# Configure XFCE, instead of directly configuring this put it in the
# .bash_profile because the command won't work without X11 running.
#
# Put in .profile because .xsessionrc isn't run by lightdm sometimes
echo "xfconf-query -c xsettings -p /Net/ThemeName -s Xfce" >> ~/.profile
echo "xfconf-query -c xsettings -p /Net/IconThemeName -s Humanity" >> ~/.profile

# Install an .xsession
echo "source .profile"  > ~/.xsession
echo "startxfce4"      >> ~/.xsession

# Create a desktop shortcut for DrRacket
echo "[Desktop Entry]"             > ~/Desktop/DrRacket.desktop
echo "Version=1.0"                >> ~/Desktop/DrRacket.desktop
echo "Type=Application"           >> ~/Desktop/DrRacket.desktop
echo "Name=DrRacket"              >> ~/Desktop/DrRacket.desktop
echo "Comment="                   >> ~/Desktop/DrRacket.desktop
echo "Exec=/home/artifact/racket/bin/drracket" >> ~/Desktop/DrRacket.desktop
echo "Icon=/home/artifact/racket/share/drracket-exe-icon.png" >> ~/Desktop/DrRacket.desktop
echo "Path="                      >> ~/Desktop/DrRacket.desktop
echo "Terminal=false"             >> ~/Desktop/DrRacket.desktop
echo "StartupNotify=false"        >> ~/Desktop/DrRacket.desktop

chmod +x ~/Desktop/DrRacket.desktop

# Center wallpaper and set bg color
echo "xfconf-query -n -t int -c xfce4-desktop -p /backdrop/screen0/monitorVBOX0/workspace0/image-style -s 1" >> ~/.profile
echo "xfconf-query -n -t uint -t uint -t uint -t uint -c xfce4-desktop -p /backdrop/screen0/monitorVBOX0/workspace0/color1 -s 65535 -s 65535 -s 65535 -s 65535" >> ~/.profile

# Cleanup
rm ~/Desktop/racket-6.9-x86_64-linux.sh
# rm -r ~/icfp-2017-artifact
