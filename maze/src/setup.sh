function usage() {
  echo "$0 <rows> <colums>"
}

export rows=$1
export columns=$2

if [ -z $rows ]
then
  usage
  exit 0
fi

if [ -z $columns ]
then
  usage
  exit 0
fi

if [ -e mazeIn ]
then
    rm mazeIn
fi

mkFifo mazeIn

if [ -e mazeOut ]
then
    rm mazeOut
fi

mkFifo mazeOut

tail -f mazeIn | ./maze $1 $2 > mazeOut &

