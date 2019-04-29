echo -e "T\taccept\c"
for T in $(seq 3.5 -0.1 1.5)
do
echo -e "\n$T\t\c"
for Time in $(seq 1 20 )
do
	file=$(echo -e "down.$T.Result.$Time.dat")
	accept=$(sed -n 2p $file  | awk '{printf"%10.3f \n",$6}')
	echo -e "$accept\t\c"
done
done

for T in $(seq 1.6 0.1 3.5)                                                                                                        
do
echo -e "\n$T\t\c"
for Time in $(seq 1 20 )     
do
	file=$(echo -e "up.$T.Result.$Time.dat")
        accept=$(sed -n 2p $file  | awk '{printf"%10.3f \n",$6}')
        echo -e "$accept\t\c"
done
done
