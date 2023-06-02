import pandas as pd

def InterpolateCurves(ValueCurve1, ValueCurve2, Curve1, Curve2, ValueWanted):
    Curve3 = []
    for i in range(len(Curve1)):
        Curve3.append(linear_interpolate(ValueCurve1, ValueCurve2, Curve1[i], Curve2[i], ValueWanted))
    return Curve3

def linear_interpolate(x0, x1, y0, y1, x):
    if x1 - x0 == 0:
        return (y0 + y1)/2
    return y0 + ((x - x0)*(y1 - y0)/(x1 - x0))


def GetInterpolatedPolarFromPolars(PolarsDF, WantReynolds):
    if WantReynolds < int(PolarsDF.columns[1]):
        Polar = PolarsDF[PolarsDF.columns[1]].tolist()
    elif WantReynolds > int(PolarsDF.columns[-1]):
        Polar = PolarsDF[PolarsDF.columns[-1]].tolist()
    else:
        for i in range(len(PolarsDF.columns) - 2):
            key1 = PolarsDF.columns[i + 1]
            key2 = PolarsDF.columns[i + 2]
            if int(key1) < WantReynolds and int(key2) > WantReynolds: 
                Polar = InterpolateCurves(int(key1), int(key2), PolarsDF[key1], PolarsDF[key2], WantReynolds)
                break
            elif int(key1) == WantReynolds:
                Polar = PolarsDF[key1].tolist()
                break
            elif int(key2) == WantReynolds:
                Polar = PolarsDF[key2].tolist()
                break
    alphas = PolarsDF["alpha"].tolist()
    return alphas, Polar

Polars = pd.read_table("TestCLPolar.dat")
x, y = GetInterpolatedPolarFromPolars(Polars, 50000)
print(y)


