#!/bin/bash

i="$(( ( RANDOM % 20 )  + 1 ))"
A="$(date +%S|awk '{for(i=1;i<=NF;i++)$i+=0}1')"
if [ $[$A%17] -eq 0 ]; then
    echo "🌕"
else
    echo "🌝"
fi
exit


# 🌎 🌍 🌏 🌕 🌖 🌗 🌘 🌑 🌒 🌓 🌔 🌚 🌝 🌞 🌛 🌜 🌙 💫 ⭐️ 🌟 ✨   ⚡️ 🔥 💥 ☄️ ☀️ 🌤 ⛅️ 🌥 🌦 🌈 ☁️ 🌧 ⛈ 🌩 🌨 ☃️ ⛄️ ❄️ 🌬 💨 🌪 🌫 🌊 💧 💦 ☔️
