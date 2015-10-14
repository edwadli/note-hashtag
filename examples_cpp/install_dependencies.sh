platform='unknown'
uname_str=`uname`
if [[ "$uname_str" == 'Linux' ]]; then
   platform='linux'
elif [[ "$uname_str" == 'Darwin' ]]; then
   platform='osx'
fi

echo -ne "Press return to install stk: "
read

if [[ $platform == 'linux' ]]; then
   sudo apt-get install stk stk-doc libstk0-dev
elif [[ $platform == 'osx' ]]; then
   brew install stk
fi
