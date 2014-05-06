import sys

items = open(sys.argv[1].strip() + "/item")
lines = items.readlines()
items.close()

cnt = len(lines)

result_file = open(sys.argv[1].strip() + "/result")
lines = result_file.readlines()
result_file.close()
numbers = []
multiple_mrs = []
restore = {}


result_file = open(sys.argv[1].strip() + "/result", 'w')

for line in lines:
	line = line.strip()
	if line.find("@") == -1: 
		print line
		continue
	each = line.split("@")
	if each[0].strip() not in numbers:
		numbers.append(each[0].strip())
		result_file.write(line + "\n")
	else:
		multiple_mrs.append(line[line.find("@"):])
		restore[line[line.find("@"):]] = each[0].strip()

if len(multiple_mrs) > 0:
	item_file = open(sys.argv[1].strip() + "/item", 'a')
	parse_file = open(sys.argv[1].strip() + "/parse", 'a')
	item_buf = ""
	parse_buf = ""

	for m in multiple_mrs:
		cnt += 1
		result_file.write(str(cnt) + m + "\n")
		item_buf += str(cnt) + "@@@@@@@@@@@@@@\n"
		parse_buf += str(cnt) + "@1@" + str(cnt) + "@0@0@0@0@0@0@0@0@0@2@0@0@0@0@0@0@0@0@0@0@0@0@0@0@0@0@0@0@0@0@0@0@0@0@0@\n"

	item_file.write(item_buf.strip())
	item_file.close()
	parse_file.write(parse_buf.strip())
	parse_file.close()

result_file.close()
