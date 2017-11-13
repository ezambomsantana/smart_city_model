timestamp() {
  date +"%T"
}

timestamp


max=100
for i in `seq 2 $max`
do


	free -m | awk 'NR==2{printf "%s, %s, %.2f%%", $3,$2,$3*100/$2 }'

	grep 'cpu ' /proc/stat | awk '{usage=($2+$4)*100/($2+$4+$5)} END {print "," usage "%"}'
	
	sleep 1m

done

# to run the script call ./run_monitoring.sh >> file_name
