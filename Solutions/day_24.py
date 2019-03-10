f = open('input_day_24.txt')

input = []
for k in f.readlines():
    link = k.replace("\n", "").split("/")   
    l    = int(link[0])
    r    = int(link[1])
    input.append([l, r, 0])

bridge  = [0]
max_w   = 0
max_l_w = 0
max_l   = 0
def connect(con):
    global max_w, max_l_w, max_l
    for i in input:
        if i[2] != 1:
            left = i[0]
            right = i[1]
            if left == con or right == con:
                i[2] = 1
                if left == con:
                    bridge.append(right)
                    connect(right)
                elif right == con:
                    bridge.append(left)
                    connect(left)
                i[2] = 0
                weight = 2 * sum(bridge) - bridge[-1]
                if weight > max_w:
                    max_w = weight
                length = len(bridge)
                if length >= max_l:
                    max_l = length
                    if weight > max_l_w:
                        max_l_w = weight  
                del bridge[-1]
                 
connect(0)

print max_w
print max_l_w
