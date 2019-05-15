import subprocess
import time

def main ():
    subprocess.Popen(["./zfsServer","-shard","0"])
    time.sleep(3)
    subprocess.Popen(["./zfsServer","-shard","1"])
    subprocess.run(["./zfsServer","-shard","2"])

main ()
