import os, re, sys, json

project_dir = sys.argv[1]
filter_regexp = sys.argv[2].split(",")

resultant_files = []
for root, dirs, files in os.walk(project_dir):
    for name in files:
        resultant_files.append(os.path.join(root, name))

for r in filter_regexp:
    resultant_files = [f for f in resultant_files if not re.compile(r).search(f)]
        
result_dict = {}
for f in resultant_files:
    if os.path.basename(f) in result_dict:
        result_dict[os.path.basename(f)].append(os.path.dirname(f) + "/")
    else:
        result_dict[os.path.basename(f)] = [os.path.dirname(f) + "/"]

print json.dumps(result_dict)
