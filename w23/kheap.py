import math
def test():
    kh = KHeap()
    v = [7, 14, 8, 16, 2, 5, 6, 13, 9, 3, 10, 11, 4, 12]
    #v = [2, 1]
    for i in range(len(v)):
        kh.sortpush(v[i], i)
        out = "Testing: \n"
        breakpoint()
        for j in range(i + 1):
            out += "%i " % kh._s[j].value
        print(out)
    print(kh)
    out = ""
    for n in kh._s:
        out += "%i " % n.value
    print(out)
class LinkIndex:
    def __init__(self, v, i):
        self.value = v
        self.index = i
class KHeap:
    def __init__(self):
        self._h = []
        self._s = []
    def sortpush(self, x, i, looped=False):
        y = (i-1)//2
        if i == 0:
            if looped:
                return
            else:
                link = LinkIndex(x, 0)
                self._h.append(link)
                self._s.append(link)
                return
        else:
            if not looped:
                link = LinkIndex(x, i)
                self._h.append(link)
                self._s.append(link)
                looped = True
            if x < self._h[y].value:
                self._h[i].value = self._h[y].value
                self._h[y].value = x
                ind_add = self._h[y].index
                ind_del = self._h[i].index
                self._h[i].index = ind_del
                self._h[y].index = ind_add
                self._s = self._s[:ind_add] + [LinkIndex(x, ind_add)] + self._s[ind_add + 1:]
                self._s = self._s[:ind_add + 1] + self._s[ind_add + 2:]
                self.sortpush(x, y, looped)
            else:
                depth = math.floor(math.log(i + 1, 2))
                cmp_start = 2**(depth - 1) - 1
                cmp_end = i
                cmp_range = cmp_end - cmp_start - 1
                while cmp_end - cmp_start - 1 > 4:
                    midrange = cmp_range//2
                    pivot = cmp_start + midrange
                    if x > self._s[pivot].value:
                        cmp_start = pivot + 1
                    elif x < self._s[pivot].value:
                        cmp_end = pivot - 1
                    else:
                        s_insert = [LinkIndex(x, i)]
                        pivot_del = self._h[i].index
                        self._s = self._s[:pivot] + s_insert + self._s[pivot:]
                        self._s = self._s[:pivot_del + 1] + self._s[pivot_del + 2:]
                        return
                    if cmp_end > len(self._s):
                        breakpoint()
                for ind in range(cmp_start, cmp_end):
                    if x < self._s[ind].value:
                        s_insert = [LinkIndex(x, i)]
                        pivot_del = self._h[i].index
                        self._h[i].index = ind
                        self._s = self._s[:ind] + s_insert + self._s[ind:]
                        self._s = self._s[:pivot_del + 1] + self._s[pivot_del + 2:]
                return
    def __str__(self):
        level = 1
        out = ""
        for i in range(len(self._h)):
            out += "%i " % self._h[i].value
            if i + 1 == 2**level - 1:
                out += '\n'
                level += 1
        return out                
