using HDF5
using DataFrames
using PyPlot
using LaTeXStrings

begin
    rcParams = PyPlot.PyDict(PyPlot.matplotlib."rcParams")
    rcParams["font.family"] = "serif"
    rcParams["text.usetex"] = true

    rcParams["font.size"] = "11"
    rcParams["xtick.labelsize"] = 11
    rcParams["ytick.labelsize"] = 11
    rcParams["backend"] = "pdf"

    rcParams["xtick.direction"] = "in"
    rcParams["xtick.top"] = true
    rcParams["ytick.right"] = true

    rcParams["ytick.direction"] = "in"
    rcParams["xtick.minor.visible"] = true
    rcParams["ytick.minor.visible"] = true
    rcParams["xtick.major.size"] = 5
    rcParams["ytick.major.size"] = 5
    rcParams["xtick.major.width"] = 0.6
    rcParams["ytick.major.width"] = 0.6
    rcParams["xtick.minor.size"] = 2
    rcParams["ytick.minor.size"] = 2
    rcParams["xtick.minor.width"] = 0.6
    rcParams["ytick.minor.width"] = 0.6
end

# The main HDF5 file contains the FE data, all correlation functions and scalar objects
main_file = "./build/output.h5"

function reload_data()
    global scalars
    global maps
    global fe

    h5open(main_file, "r") do file
        println(keys(file))
        sg = file["scalars"]
        println(keys(sg))
        time = read(sg["time"])
        global scalars = DataFrame([read(sg[s])[1:length(time)] for s in keys(sg)], keys(sg))
        mg = file["maps"]
        global maps = Dict(s => sort(read(mg[s]), by=x -> parse(Int64, x)) for s in keys(mg))
        #global fe = [file["FE"][s] for s in keys(file["FE"])]
        #fe = [Dict(s => read(f[s]) for s in keys(f)) for f in fe]
        nothing
    end
end


# plot m2A over flow time
begin
    reload_data()
    m2A = scalars."m2A"
    k = scalars."k"

    fig, ax = subplots(figsize=(4, 3), dpi=150)

    ax.plot(k, m2A, label=L"m_A^2")
    ax.set_xlabel(L"k")
    ax.set_ylabel(L"m_A^2")
    ax.set_xscale("log")

    # make inset to zoom in at small k flow time
    inset_ax = fig.add_axes([0.3, 0.3, 0.35, 0.35])
    inset_ax.plot(k, m2A)
    inset_ax.set_xlim(0.0001, 1.0)
    inset_ax.set_ylim(-0.04999, 0.15)
    inset_ax.set_xscale("log")

    # insert text of final m2A

    fig
end

# Approximate scale-setting to have the peak at 1GeV

using DelimitedFiles

begin
    # load csv data from lattice
    #lat2_dat = np.loadtxt("gluon_prop_R4GeV_128p4_b6p3.dat")
    lat2_dat = readdlm("gluon_prop_R4GeV_128p4_b6p3.dat")
    p_lat2 = lat2_dat[1:end, 1]
    GA_lat2 = lat2_dat[1:end, 2]
    ZAm1_lat2 = GA_lat2 .* p_lat2 .^ 2
    err_lat2 = lat2_dat[1:end, 3]
    ZAm1_lat2_err = err_lat2 .* p_lat2 .^ 2

    x_resc = 1.6
    y_resc = 2.0

    ZA = maps["ZA"][((x->x).(keys(maps["ZA"]))[end])]
    coord_ZA = ZA["coordinates"]
    dat_ZA = y_resc * ZA["data"]
    pGeV = x_resc * map(x -> x[1], coord_ZA[:])

    Zc = maps["Zc"][((x->x).(keys(maps["Zc"]))[end])]
    coord_Zc = Zc["coordinates"]
    dat_Zc = Zc["data"]
    pGeV = x_resc * map(x -> x[1], coord_Zc[:])

    fig2, (ax1, ax2, ax3) = subplots(1, 3, figsize=(3 * 3, 2.5), dpi=300)

    ax1.errorbar(p_lat2, ZAm1_lat2, yerr=ZAm1_lat2_err, fmt="o", label="Sternbeck et al. (2006)", color="black", markersize=0.5, capsize=2)
    ax1.plot(pGeV, dat_ZA .^ -1, label=L"Z_A", color=plt.cm.tab10(3), zorder=10)
    ax1.set_xlabel(L"p\textrm{ [GeV]}")
    ax1.set_ylabel(L"1/Z_A")
    ax1.set_xscale("log")
    ax1.set_xlim(1e-4 * x_resc, 100 * x_resc)
    ax1.set_xlim(1.00000001e-2, 10)
    ax1.set_ylim(-0.03, 2.7)

    ax2.plot(pGeV, dat_Zc, label=L"Z_c", color=plt.cm.tab10(0))
    ax2.set_xlabel(L"p\textrm{ [GeV]}")
    ax2.set_ylabel(L"Z_c")
    ax2.set_xscale("log")
    ax2.set_yscale("log")
    ax2.set_xlim(1e-4 * x_resc, 100 * x_resc)

    ZA = maps["ZA"][((x->x).(keys(maps["ZA"]))[end])]
    coord_ZA = ZA["coordinates"]
    dat_ZA = ZA["data"]
    pGeV = x_resc * map(x -> x[1], coord_ZA[:])

    Zc = maps["Zc"][((x->x).(keys(maps["Zc"]))[end])]
    coord_Zc = Zc["coordinates"]
    dat_Zc = Zc["data"]
    pGeV = x_resc * map(x -> x[1], coord_Zc[:])

    ZA3 = maps["ZA3"][((x->x).(keys(maps["ZA3"]))[end])]
    coord_ZA3 = ZA3["coordinates"]
    dat_ZA3 = ZA3["data"]
    pGeV = x_resc * map(x -> x[1], coord_ZA3[:])

    ZA4 = maps["ZA4"][((x->x).(keys(maps["ZA4"]))[end])]
    coord_ZA4 = ZA4["coordinates"]
    dat_ZA4 = ZA4["data"]
    pGeV = x_resc * map(x -> x[1], coord_ZA4[:])

    ZAcbc = maps["ZAcbc"][((x->x).(keys(maps["ZAcbc"]))[end])]
    coord_ZAcbc = ZAcbc["coordinates"]
    dat_ZAcbc = ZAcbc["data"]
    pGeV = x_resc * map(x -> x[1], coord_ZAcbc[:])

    gA3 = (dat_ZA3 .^ 2 ./ (dat_ZA .^ 3)) ./ (4π)
    gA4 = (dat_ZA4 ./ (dat_ZA .^ 2.0)) ./ (4π)
    gAcbc = (dat_ZAcbc .^ 2 ./ (dat_ZA .* dat_Zc .^ 2)) ./ (4π)

    ax3.plot(pGeV, gA3, label=L"\alpha_{A3}", color=plt.cm.tab10(3))
    ax3.plot(pGeV, gA4, label=L"\alpha_{A4}", color=plt.cm.tab10(1))
    ax3.plot(pGeV, gAcbc, label=L"\alpha_{A\bar{c}c}", color=plt.cm.tab10(0))
    ax3.set_xlabel(L"p\textrm{ [GeV]}")
    #ax3.set_ylabel(L"g")
    ax3.set_xscale("log")
    ax3.set_yscale("log")
    ax3.legend(frameon=false)
    ax3.set_ylim(5e-5, 6)
    ax3.set_xlim(1e-4 * x_resc, 100 * x_resc)

    fig2.tight_layout()
    fig2.subplots_adjust(wspace=0.3)
    ax1.legend(frameon=false, loc="upper left", reverse=true, fontsize=9)

    fig2.savefig("YM.pdf", bbox_inches="tight")
    fig2
end