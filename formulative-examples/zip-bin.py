import glob, os

for f in glob.glob("bin/*"):
    print(f)
    if not ((f == "bin/harmonic-oscillator") or (f == "bin/lorenz")):
        os.remove(f)

os.system("rm -rf formulative-examples-bin.zip")

os.system("zip -r formulative-examples-bin bin/")
