
timestamp() {
  date +"%T"
}

timestamp


max=100
for i in `seq 2 $max`
do
	free -m | awk 'NR==2{printf "%s, %s, %.2f%%", $3,$2,$3*100/$2 }'

	top -bn1 | grep load | awk '{printf ",%.2f\n", $(NF-2)}' 
	
	sleep 1m

done

# to run the script call ./run_monitoring.sh >> file_name
