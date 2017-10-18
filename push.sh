#!/bin/bash
if [ ! -d "home" ]; then
    mkdir home
fi
if [ ! -d "home/lee/" ]; then
    mkdir home/lee/
fi
cp /home/lee/.config ./home/lee/ -r
cp /home/lee/.xinitrc /bigD/git/archcfg/home/lee/
cp /home/lee/.Xresources ./home/lee/
cp /home/lee/.zshrc ./home/lee/
cp /home/lee/.emacs ./home/lee/
#cp /home/lee/.emacs.d ./home/lee/.emacs.d -r

if [ ! -d "home/lee/.config/chromium" ]; then
    sleep 0.1
    echo "home/lee/.config/chromium 目录不存在"
fi
    sleep 0.1
    echo "home/lee/.config/chromium 目录存在"
    echo "执行删除"
    rm home/lee/.config/chromium -r


if [ ! -d "home/lee/.config/kuwo" ]; then
    sleep 0.1
    echo "home/lee/.config/kuwo 目录不存在"
fi
    sleep 0.1
    echo "home/lee/.config/kuwo 目录存在"
    echo "执行删除"
    rm home/lee/.config/kuwo -r

if [ ! -d "home/lee/.config/VirtualBox" ]; then
    sleep 0.1
    echo "home/lee/.config/VirtualBox 目录不存在"
fi
    sleep 0.1
    echo "home/lee/.config/VirtualBox 目录存在"
    echo "执行删除"
    rm home/lee/.config/VirtualBox -r

if [ ! -d "home/lee/.emacs.d" ]; then
    sleep 0.1
    echo "home/lee/.emacs.d 目录不存在"
fi
    sleep 0.1
    echo "home/lee/.emacs.d 目录存在"
    echo "执行删除"
    rm home/lee/.emacs.d -r

    if [ ! -d "home/lee/.config/chromium" ]; then
	if [ ! -d "home/lee/.config/kuwo" ]; then
	    if [ ! -d "home/lee/.config/VirtualBox" ]; then
		if [ ! -d "home/lee/.emacs.d" ]; then
		sleep 1
		echo "清理完毕"
		fi
	    fi
	fi
    fi

echo "*******************************************************************"

    echo -n "Enter yes to update the archcfg:"
    read  yes
    if [ $yes=yes ]; then
	echo "执行文件上传"   
	git add .
	git commit -m "2017"
	git push
    fi
    exit 0
