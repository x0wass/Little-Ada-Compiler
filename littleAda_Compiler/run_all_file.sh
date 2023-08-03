#!/bin/bash
path=$1
ok="$path/OK"
ko="$path/KO"

echo ">>>>>>>>>> OK <<<<<<<<<"
for i in $(ls $ok)
do
    echo "****** $i ******"
    $(./test_parser < $ok"/"$i)
    echo "**** END $i ****"
done

echo ">>>>>>>>>> KO <<<<<<<<<"
for i in $(ls $ko)
do
    echo "****** $i ******"
    echo $(./test_parser < $ko"/"$i)
    echo "**** END $i ****"
done


