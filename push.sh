#!/bin/bash
if [ ! -d "home" ]; then
    mkdir /bigD/git/archcfg/home
fi
if [ ! -d "home/lee/" ]; then
    mkdir /bigD/git/archcfg/home/lee/
fi
cp /home/lee/.config /bigD/git/archcfg/home/lee/ -r
cp /home/lee/.xinitrc /bigD/git/archcfg/home/lee/
cp /home/lee/.Xresources /bigD/git/archcfg/home/lee/
cp /home/lee/.zshrc /bigD/git/archcfg/home/lee/
cp /home/lee/.emacs /bigD/git/archcfg/home/lee/
#cp /home/lee/.emacs.d ./home/lee/.emacs.d -r

if [ ! -d "/bigD/git/archcfg/home/lee/.config/chromium" ]; then
    sleep 0.1
    echo "/bigD/git/archcfg/home/lee/.config/chromium 目录不存在"
fi
    sleep 0.1
    echo "/bigD/git/archcfg/home/lee/.config/chromium 目录存在"
    echo "执行删除"
    rm /bigD/git/archcfg/home/lee/.config/chromium -r


if [ ! -d "/bigD/git/archcfg/home/lee/.config/kuwo" ]; then
    sleep 0.1
    echo "/bigD/git/archcfg/home/lee/.config/kuwo 目录不存在"
fi
    sleep 0.1
    echo "/bigD/git/archcfg/home/lee/.config/kuwo 目录存在"
    echo "执行删除"
    rm /bigD/git/archcfg/home/lee/.config/kuwo -r

if [ ! -d "/bigD/git/archcfg/home/lee/.config/VirtualBox" ]; then
    sleep 0.1
    echo "/bigD/git/archcfg/home/lee/.config/VirtualBox 目录不存在"
fi
    sleep 0.1
    echo "/bigD/git/archcfg/home/lee/.config/VirtualBox 目录存在"
    echo "执行删除"
    rm /bigD/git/archcfg/home/lee/.config/VirtualBox -r

if [ ! -d "/bigD/git/archcfg/home/lee/.emacs.d" ]; then
    sleep 0.1
    echo "/bigD/git/archcfg/home/lee/.emacs.d 目录不存在"
fi
    sleep 0.1
    echo "/bigD/git/archcfg/home/lee/.emacs.d 目录存在"
    echo "执行删除"
    rm home/lee/.emacs.d -r

    if [ ! -d "/bigD/git/archcfg/home/lee/.config/chromium" ]; then
	if [ ! -d "/bigD/git/archcfg/home/lee/.config/kuwo" ]; then
	    if [ ! -d "/bigD/git/archcfhome/lee/.config/VirtualBox" ]; then
		if [ ! -d "/bigD/git/archcfg/home/lee/.emacs.d" ]; then
		sleep 1
		echo "清理完毕"
		fi
	    fi
	fi
    fi

    echo "*****************************执行文件上传****************************"
    cd /bigD/git/archcfg
	git add .
	git commit -m "2017"
	git push
	
    exit 0
