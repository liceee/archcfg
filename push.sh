#!/bin/bash
echo "运行脚本"
echo $0 $1 
echo "*****************************afp 备份文件中****************************"

if [ ! -d "/bigD/git/archcfg/home/" ]; then
    mkdir /bigD/git/archcfg/home/
fi
if [ ! -d "/bigD/git/archcfg/home/lee/" ]; then
    mkdir /bigD/git/archcfg/home/lee/
fi
if [ ! -d "/bigD/git/archcfg/etc/" ]; then
    mkdir /bigD/git/archcfg/etc/ -p
fi

cp /etc/mpd.conf /bigD/git/archcfg/etc/
cp /home/lee/.config/i3 /bigD/git/archcfg/home/lee/.config/ -r
cp /home/lee/.config/fcitx /bigD/git/archcfg/home/lee/.config/ -r
cp /home/lee/.config/tilda /bigD/git/archcfg/home/lee/.config/ -r
cp /home/lee/.xinitrc /bigD/git/archcfg/home/lee/
cp /home/lee/.Xresources /bigD/git/archcfg/home/lee/
cp /home/lee/.zshrc /bigD/git/archcfg/home/lee/
cp /home/lee/.emacs /bigD/git/archcfg/home/lee/
cp /home/lee/.bashrc /bigD/git/archcfg/home/lee/
#cp /home/lee/.emacs.d ./home/lee/.emacs.d -r

if [ -d "/bigD/git/archcfg/home/lee/.config/chromium" ]; then
    sleep 0.1
    echo "/bigD/git/archcfg/home/lee/.config/chromium 目录存在"
    echo "执行删除"
    rm /bigD/git/archcfg/home/lee/.config/chromium -r
fi


if [ -d "/bigD/git/archcfg/home/lee/.config/kuwo" ]; then
    sleep 0.1
    echo "/bigD/git/archcfg/home/lee/.config/kuwo 目录存在"
    echo "执行删除"
    rm /bigD/git/archcfg/home/lee/.config/kuwo -r
fi

if [ -d "/bigD/git/archcfg/home/lee/.config/VirtualBox" ]; then
    sleep 0.1
    echo "/bigD/git/archcfg/home/lee/.config/VirtualBox 目录存在"
    echo "执行删除"
    rm /bigD/git/archcfg/home/lee/.config/VirtualBox -r
fi

if [ -d "/bigD/git/archcfg/home/lee/.emacs.d" ]; then
    sleep 0.1
    echo "/bigD/git/archcfg/home/lee/.emacs.d 目录存在"
    echo "执行删除"
    rm home/lee/.emacs.d -r
fi

if [ -d "/bigD/git/archcfg/home/lee/.cache" ]; then
    sleep 0.1
    echo "/bigD/git/archcfg/home/lee/.cache 目录存在"
    echo "执行删除"
    rm home/lee/.cache -r
fi

    if [ ! -d "/bigD/git/archcfg/home/lee/.config/chromium" ]; then
	if [ ! -d "/bigD/git/archcfg/home/lee/.config/kuwo" ]; then
	    if [ ! -d "/bigD/git/archcfhome/lee/.config/VirtualBox" ]; then
		if [ ! -d "/bigD/git/archcfg/home/lee/.emacs.d" ]; then
		sleep 0.1
		echo "备份工作以完成"
#		tree /bigD/git/archcfg/
		fi
	    fi
	fi
    fi
    du -h /bigD/git/archcfg
    echo
    echo
    echo
    if [ -n "$1" ]; then
	echo "执行提交步骤"
	cd /bigD/git/archcfg
	git add .
	git commit -m "$1"
	git push
    fi
    exit 0
