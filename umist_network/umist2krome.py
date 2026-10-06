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
body += "@common:user_Auv,user_alb,user_xi,user_AuvAv,user_zeta,user_V"
if IP or AP: body += ",user_rscale"
if IP: body += ",user_Gstar,user_Auv_star"
if AP: body += ",user_Gcomp,user_Auv_comp,user_rbinscale"
body += "\n"
body += "@var:xCO = n(idx_CO) / get_Hnuclei(n(:))\n"
count = 0
rows = open(fname_umist)

# find gamma_co
for row in rows:
    srow = row.strip()
    if srow and not srow.startswith("#") and srow.split(":")[2] == "CO" and srow.split(":")[1] == "PH":
        gamma_CO = float(srow.split(":")[11])
        break

def strip_row(row):
    srow = row.strip()
    if srow == "" or srow.startswith("#"): return None
    arow = srow.split(":")
    rtype = arow[1]
    rr = arow[2:4]
    pp = arow[4:8]
    ka, kb, kc = [float(x.replace(',', '.')) for x in arow[9:12]]
    tmin, tmax = [float(x.replace(',', '.')) for x in arow[12:14]]
    comment = arow[17]
    return rtype, rr, pp, ka, kb, kc, tmin, tmax, comment

rows.seek(0)
for row in rows:
    stripped = strip_row(row)
    if stripped is None: continue
    rtype, rr, pp, ka, kb, kc, tmin, tmax, comment = stripped

    rate = None
    if rtype == "CR":
        rate = f"user_zeta * {ka:.2e} * (Tgas / 3.0e2)**({kb:.2f}) * (1./(1.-user_alb)) * ({kc:.2f})"
    elif rtype == 'CP':
        rate = f"user_zeta * {ka:.2e}"
    elif rtype == "PH":
        if rr[0] == "CO": # Add shielding to the CO reaction
            frace = 1.0 / 3.0        # fractional population of lower level
            fosce = 0.017            # effective dissociative oscillator strength
            lamdae = 1000.0 * 1.0e-8 # effective wavelength (in cm)
            bands = 1.0              # effective number of bands
            ge0 = 2.4e-10            # unshielded photodissociation rate of co
            h2col = f"user_Auv / user_AuvAv * 1.87e21"
            taue = f"{1.5 * 0.0265* frace * fosce * lamdae:.2e} * {h2col} * xCO / user_V"
            rate = f"{ge0*bands:.2e} * exp(-1.644 * user_Auv**0.86) * (1 - exp(-{taue})) / ({taue})"
        # elif rr[0] == "H2": # Add shielding to the H2 reaction (deactivate the H2 photodissociation reaction)
        #     rate = "0"
        else:
            rate = f"{ka:.2e} * user_xi * exp(({gamma_CO:.2f} - {kc:.2f}) * user_Auv / user_AuvAv)"
    elif rtype in ["IP", "AP"]:
        continue
    else:
        rate = f"{ka:.2e}"
        if kb != 0e0:
            rate += f" * (Tgas / 3.0e2)**({kb:.2f})"
        if kc != 0e0:
            rate += f" * exp(-{kc:.2f} / Tgas)"

    body += f"{count},{','.join(rr)},{','.join(pp)},{tmin:.2e},{tmax:.2e},{rate}\n"
    count += 1

if IP:
    for row in open("IP.rates"):
        stripped = strip_row(row)
        if stripped is None: continue
        rtype, rr, pp, ka, kb, kc, tmin, tmax, comment = stripped
        rate = f"user_rscale * {ka:.2e} * exp(-{kc:.2f} * user_Auv_star / user_AuvAv)"
        if not ("CWLeo" in comment or "Millar 2018" in comment):
            rate = f"user_Gstar * {rate}"
        body += f"{count},{','.join(rr)},{','.join(pp)},{tmin:.2e},{tmax:.2e},{rate}\n"
        count += 1

if AP:
    for row in open(f"AP_{AP}K.rates"):
        stripped = strip_row(row)
        if stripped is None: continue
        rtype, rr, pp, ka, kb, kc, tmin, tmax, comment = stripped
        rate = f"user_rscale * {ka:.2e} * exp(-{kc:.2f} * user_Auv_comp / user_AuvAv)"
        if "CWLeo" in comment:
            rate = f"user_rbinscale * {rate}"
        elif "Millar 2018" in comment:
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
    body = body.replace(f",{s},", ",,")

with open(fname, "w") as f:
    f.write(body)

print(f"Wrote {count} reactions to {fname}")
