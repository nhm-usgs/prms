import os
import sys

exe_name = "prms"
if sys.platform == "win32":
    exe_name += ".exe"
fpth = os.path.join("..", "..", "bin", exe_name)

assert os.path.isfile(fpth), f"{fpth} does not exist"
assert os.access(fpth, os.X_OK), f"{fpth} is not executable"
