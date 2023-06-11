#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
List MarkovTraceAndValues(NumericMatrix transitions, List values, NumericVector init, int ncycles, int nstates, int nvalues, double ccons) {
    Rcpp::NumericMatrix trace(ncycles + 1, nstates);
    Rcpp::NumericVector results(ncycles * nstates * nvalues);
    // Rcout << "The number of rows: " << trace.nrow() << "\n";
    // Rcout << "The number of columns: " << trace.ncol() << "\n";
    int transrows = transitions.nrow();
    Rcpp::NumericMatrix uncondtransprod(transrows, 3);
    //Rcpp::NumericMatrix outcomes();

    for(int i = 0; i < nstates; i++) {
        trace(0, i) = init[i];
    }

    int from;
    int last_from;
    int to;
    int cycle;
    double value;
    double prevtracevalue;
    double currtracevalue;
    double cumtransprod = 0;
    int cconsfound = 0;

    for (int i = 0; i < transrows; i++) {
        // https://stackoverflow.com/questions/50279080/rcpp-containers-do-not-release-memory
        // use standard vectors instead?
        // or maybe not since this shouldn't create copies anyway https://teuder.github.io/rcpp4everyone_en/140_dataframe.html#accessing-dataframe-elements
        // fromcol = transitions["from"];
        // from = fromcol[i];
        // tocol = transitions["to"];
        // Rcout << "Check matrix accessing : " << transitions(0,0) << "\n";

        from = int (transitions(i, 1)) - 1;
        cycle = int (transitions(i, 0));
        prevtracevalue = trace(cycle - 1, from);
        value = transitions(i, 3);
        //Rcout << "Row : " << i << "\n";
        if ((i > 0) && (from != last_from)) {
            
            //Rcout << "NEW FROM STATE" << "\n";
            cumtransprod = value;
            cconsfound = 0;
        } else {
            if (value == ccons) {
                value = 1 - cumtransprod;
                transitions(i, 3) = value;
                cconsfound++;
                //Rcout << "Found C" << "\n";
                if (cconsfound > 1) {
                    stop("C may only be used once per from state");
                }
            } else {
                //Rcout << "Adding to cumulative: " << value << "\n";
                cumtransprod += value;
                if (cumtransprod > 1) {
                    stop("transition probabilities may not sum to >1");
                }
            }
        }

            //Rcout << "\n" << "\n" << "___________" << "\n" << "\n";
        //cyclecol = transitions["cycle"];
        to = int (transitions(i, 2)) - 1;

        // Rcout << "To State : " << to << "\n";

        // Rcout << "From State : " << from << "\n";
        // valueecol = transitions["value"];
        // value = valueecol[i];
        currtracevalue = trace(cycle, to);
        // Rcout << "Setting row : " << (cycle) << "\n";
        // Rcout << "Setting col : " << to << "\n";
        // Rcout << "trans prob : " << value << "\n";
        // Rcout << "from state prob : " << prevtracevalue << "\n";
        // Rcout << "to state prob before this trans : " << currtracevalue << "\n";
        // Rcout << "Setting value : " << (currtracevalue + (value * prevtracevalue)) << "\n";
        double tracecomponent = value * prevtracevalue;
        uncondtransprod(i, 0) = to;
        uncondtransprod(i, 1) = from;
        uncondtransprod(i, 2) = tracecomponent;
        trace(cycle, to) = currtracevalue + tracecomponent;

        last_from = from;
      
    }

    //int index = 0;
    NumericVector valuevector;
    List valueslist;
    for (int i = 0; i < nstates; i++) {
        valueslist = values(i);
        if (valueslist.length() != 0) {
            for (int j = 0; j < nvalues; j++) {
                valuevector = valueslist(j);
                int valuevectorlength = valuevector.length();
                double valuevalue = valuevector(0);
                for (int k = 0; k < ncycles; k++) {
                    if (valuevectorlength > 1) {
                        valuevalue = valuevector(k);
                    }

                    // if (k < 3) {
                    //     Rcout << "state : " << i << "\n";
                    //     Rcout << "value : " << j << "\n";
                    //     Rcout << "cycle : " << k << "\n";
                    //     Rcout << "value of value : " << valuevalue << "\n";
                    //     Rcout << "trace: " << trace(k, i) << "\n";
                    //     Rcout << "result of value: " << (trace(k, i) * valuevalue) << "\n";
                    //     Rcout << "writing to index : " << (k + j * ncycles + k * (ncycles * nstates)) << "\n";
                    //     Rcout << "__________" << "\n";
                    //     Rcout << "__________" << "\n";
                    // }
                    results(k + i * ncycles + j * (ncycles * nstates)) = (trace(k, i) + trace(k + 1, i)) / 2 * valuevalue;
                }
            }
        }
    }

    IntegerVector dim = {ncycles, nstates, nvalues};
    results.attr("dim") = dim;

    return List::create(trace, uncondtransprod, results, transitions);
}