import os, re, sys, json

filter_regexp = sys.argv[2].split(",")
project_file = sys.argv[1]
project_json = json.loads(open(project_file).read())

project_dict = { project_json['projectId']: project_json['project'] }
for lib in project_json['libs']:
    project_dict[ lib['id'] ] = [ lib ]
    
resultant_dict = {}
for project_id, project in project_dict.iteritems():
    resultant_dict[project_id] = []
    for project_dir in project:
        for root, dirs, files in os.walk(os.path.expanduser(project_dir['dir'])):
            for name in files:
                resultant_dict[project_id].append(os.path.join(root, name))
                
for r in filter_regexp:
    regex = re.compile(r)
    for project_id, resultant_files in resultant_dict.iteritems():
        resultant_dict[project_id] = [f for f in resultant_files if not regex.search(f)]

for project_id, resultant_files in resultant_dict.iteritems():
    result_dict = {}
    for f in resultant_files:
        if os.path.basename(f) in result_dict:
            result_dict[os.path.basename(f)].append(os.path.dirname(f) + "/")
        else:
            result_dict[os.path.basename(f)] = [os.path.dirname(f) + "/"]
    resultant_dict[project_id] = result_dict
            
print json.dumps(resultant_dict)
