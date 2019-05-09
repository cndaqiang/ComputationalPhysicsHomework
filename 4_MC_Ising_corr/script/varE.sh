echo -e "T\tvarE\c"
for T in $(seq 3.5 -0.1 1.5)
do
echo -e "\n$T\t\c"
for Time in $(seq 1 20 )
do
	file=$(echo -e "down.$T.Result.$Time.dat")
	varE=$(sed -n 2p $file  | awk '{printf"%10.3f \n",$4}')
	echo -e "$varE\t\c"
done
done

for T in $(seq 1.6 0.1 3.5)                                                                                                        
do
echo -e "\n$T\t\c"
for Time in $(seq 1 20 )     
do
	file=$(echo -e "up.$T.Result.$Time.dat")
        varE=$(sed -n 2p $file  | awk '{printf"%10.3f \n",$4}')
        echo -e "$varE\t\c"
done
done
