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
body += "@common:user_Auv,user_alb,user_xi,user_AuvAv,user_velocity,user_CO_abundance"
if IP or AP: body += ",user_rscale"
if IP: body += ",user_Gstar,user_Auv_star"
if AP: body += ",user_Gcomp,user_Auv_comp,user_rbinscale"
body += "\n"
count = 0
rows = open(fname_umist)

# find gamma_co
for row in rows:
    srow = row.strip()
    if srow and not srow.startswith("#") and srow.split(":")[2] == "CO" and srow.split(":")[1] == "PH":
        gamma_CO = float(srow.split(":")[11])
        break

rows.seek(0)
for row in rows:
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
        rate = "%.2e * user_xi * exp((%.2f - %.2f) * user_Auv / user_AuvAv)" % (ka, gamma_CO, kc)
        if rr[0] == "CO": # CO photodissociation
            frace = 1.0 / 3.0        # fractional population of lower level
            fosce = 0.017            # effective dissociative oscillator strength
            lamdae = 1000.0 * 1.0e-8 # effective wavelength (in cm)
            bands = 1.0 # effective number of bands
            ge0 = 2.4e-10 # unshielded photodissociation rate of co
            # h2col = auv / auv_av * 1.87e21 # calculate h2 column density
            # xco = abundance(krome_idx_CO) # fractional abundance of co
            # v = 17.5e5 # velocity (in cm/s)
            # gammad = exp(-1.644 * auv**0.86) # calculate continuum shielding by dust (morris and jura)
            # taue = 0.0265 * frace * fosce * lamdae * h2col * xco / v # calculate effective optical depth of co at radius
            # betae = (1.0 - exp(-1.5 * taue)) / (1.5 * taue) # morris/jura approximation to the full integral
            # getcor = ge0 * betae * gammad * bands # calculate co photodissociation rate
            rate = f"{ge0*bands:.2e} * exp(-1.644 * user_Auv**0.86) * (1 - exp(-1.5 * {0.0265 * frace * fosce * lamdae * 1.87e21:.2e} * user_Auv / user_AuvAv * user_CO_abundance / user_velocity)) / (1.5 * {0.0265 * frace * fosce * lamdae * 1.87e21:.2e} * user_Auv / user_AuvAv * user_CO_abundance / user_velocity)"
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
