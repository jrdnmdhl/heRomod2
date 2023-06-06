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
        // for (int j = 0; j < nvalues; j++) {

        //     //Rcout << "State " << (to + 1) << " of " << values.length() << "\n";
        //     List values_list = values(to);
        //     //Rcout << "Value " << (j + 1) << " of " << values_list.length() << "\n";
        //     if (values_list.length() > 0) {
        //         double value = 0;
        //         NumericVector valuevector = values_list(j);
        //         int valuelength = valuevector.length();
        //         if (valuelength > 0) {
        //             if (valuelength > 1) {
        //                 //Rcout << "Cycle " << cycle << " of " << valuevector.length() << "\n";
        //                 value = valuevector(cycle - 1);
        //             } else {
        //                 value = valuevector(0);
        //             }

        //             //Rcout << "Writing to index " << (j + nvalues * to + (nvalues + nstates) * (cycle - 1)) << " of " << results.length() << "\n";
        //             results(j + nvalues * to + (nvalues + nstates) * (cycle - 1)) += tracecomponent * value;
        //         }
        //     }
        // }
        // Rcout << " value is now : " << trace(cycle, to) << "\n";
        last_from = from;
      
    }

    //int index = 0;
    for (int i = 0; i < nstates; i++) {
        List valueslist = values(i);
        if (valueslist.length() != 0) {
            for (int j = 0; j < nvalues; j++) {
                NumericVector valuevector = valueslist(j);
                int valuevectorlength = valuevector.length();
                // if (valuevectorlength == 1) {
                //     valuevector = rep_len(valuevector, ncycles);
                // }
                // //double value = valuevector(0);
                // int startrange = index;
                // int endrange = index + ncycles - 1;
                // index += ncycles;
                // Range rng = Range(startrange, endrange);
                // //NumericVector rangevector = results(rng);
                // //rangevector = valuevector * trace(_,i);
                // //Rcout << "Setting values " << startrange << " to " << endrange << "\n";
                // NumericMatrix tracesubmatrix = trace(Range(0, ncycles),_);
                // NumericVector tracecol = tracesubmatrix(_, i);

                // //Rcout << "Trace vector length:  " << tracecol.length() << "\n";
                // //Rcout << "Values vector length:  " << valuevector.length() << "\n";
                // results[rng] = valuevector * tracecol;
                // free(tracesubmatrix);
                // free(tracecol);
                // free(rng);
                // free(valuevector);

                for (int k = 0; k < ncycles; k++) {
                    if (valuevectorlength > 1) {
                        value = valuevector(k);
                    }
                    results(i + j * nstates + k * (nvalues + nstates)) = trace(k, i) * value;
                }
            }
        }
    }

    return List::create(trace, uncondtransprod, results);
}