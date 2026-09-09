import sys

# read UMIST 2022 rates and convert them to KROME format
IP = False # Internal photons
AP = False # Accretion photons of a companion

network = sys.argv[1]
if len(sys.argv) > 2:
    for arg in sys.argv[2:]:
        if "-IP" in arg:
            IP = True
        if "-AP" in arg:
            AP = int(arg[4:])

fname_umist = network+'.rates'
fname = "network_umist.dat"  # output file

skip = ["PHOTON", "CRPHOT", "CRP", "INPHOTON", "ACPHOTON"]
body = "@format:idx,R,R,P,P,P,P,tmin,tmax,rate\n"
body += "@common:user_Auv,user_alb,user_xi,user_AuvAv,user_gamma_CO,user_CO_shielding"
if IP or AP: body += ",user_rscale"
if IP: body += ",user_Gstar,user_Auv_star"
if AP: body += ",user_Gcomp,user_Auv_comp,user_rbinscale"
body += "\n"
count = 0
for row in open(fname_umist):
    srow = row.strip()
    if srow == "" or srow.startswith("#"): continue
    arow = srow.split(":")
    rtype = arow[1]
    rr = arow[2:4]
    pp = arow[4:8]
    ka, kb, kc = [float(x) for x in arow[9:12]]
    tmin, tmax = [float(x) for x in arow[12:14]]
    rate = None
    if rtype == "CR":
        rate = "%.2e * (Tgas / 3.0e2)**(%.2f) * (1./(1.-user_alb)) * (%.2f)" % (ka, kb, kc)
    elif rtype == 'CP':
        rate = "%.2e " % ka
    elif rtype == "PH":
        rate = "%.2e * user_xi * exp((user_gamma_CO - %.2f) * user_Auv / user_AuvAv)" % (ka, kc)
        if rr[0] == "CO":
            rate = "user_CO_shielding"
    elif rtype in ["IP", "AP"]:
        rate = "0"
    else:
        rate = "%.2e" % ka
        if kb != 0e0:
            rate += " * (Tgas / 3.0e2)**(%.2f)" % kb
        if kc != 0e0:
            rate += " * exp(-%.2f / Tgas)" % kc

    body += f"{count},{','.join(rr)},{','.join(pp)},{tmin:.2e},{tmax:.2e},{rate}\n"
    count += 1

if IP:
    for row in open("IP.rates"):
        srow = row.strip()
        if srow == "" or srow.startswith("#"): continue
        arow = srow.split(":")
        rtype = arow[1]
        rr = arow[2:4]
        pp = arow[4:8]
        ka, kb, kc = [float(x.replace(',', '.')) for x in arow[9:12]]
        tmin, tmax = [float(x.replace(',', '.')) for x in arow[12:14]]
        rate = f"user_rscale * {ka:.2e} * exp(-{kc:.2f} * user_Auv_star / user_AuvAv)"
        if not "CWLeo" in arow[17]:
            rate = f"user_Gstar * {rate}"
        body += f"{count},{','.join(rr)},{','.join(pp)},{tmin:.2e},{tmax:.2e},{rate}\n"
        count += 1

if AP:
    for row in open(f"AP_{AP}K.rates"):
        srow = row.strip()
        if srow == "" or srow.startswith("#"): continue
        arow = srow.split(":")
        rtype = arow[1]
        rr = arow[2:4]
        pp = arow[4:8]
        ka, kb, kc = [float(x.replace(',', '.')) for x in arow[9:12]]
        tmin, tmax = [float(x.replace(',', '.')) for x in arow[12:14]]
        rate = f"user_rscale * {ka:.2e} * exp(-{kc:.2f} * user_Auv_comp / user_AuvAv)"
        if "CWLeo" in arow[17]:
            rate = f"user_rbinscale * {rate}"
        elif "Millar 2018" in arow[17]:
            if AP == 4000:
                rate = f"user_rbinscale * {rate}"
            elif AP == 10000:
                rate = f"user_Gcomp * {rate}"
        else:
            rate = f"user_Gcomp * {rate}"
        body += f"{count},{','.join(rr)},{','.join(pp)},{tmin:.2e},{tmax:.2e},{rate}\n"
        count += 1

body = body.replace(",e-,", ",E,")

for s in skip:
    body = body.replace(",%s," % s, ",,")

with open(fname, "w") as f:
    f.write(body)

print("Wrote %d reactions to %s" % (count, fname))
