using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace JSMOptions
{
    public enum Style
    {
        European,
        American
    };

    public class Option
    {
        private readonly double xshortRate;
        private readonly double xdividendRate;
        private readonly double xvolatility;
        private readonly double xunderlyingPrice;
        private readonly double xstrikePrice;
        private readonly double xterm;
        private readonly Style xamerican;

        private const int MAX_ITER = 25;
        private const double EPSILON = 1.0E-10;

        public Option(double shortRate, double dividendRate, double volatility, double underlyingPrice, double strikePrice, double term, Style american)
        {
            this.xshortRate = shortRate;
            this.xdividendRate = dividendRate;
            this.xvolatility = volatility;
            this.xunderlyingPrice = underlyingPrice;
            this.xstrikePrice = strikePrice;
            this.xterm = term;
            this.xamerican = american;
        }

        static double CNDistPrime(double x)
        {
	        return 0.3989422804 * Math.Exp(-x * x / 2.0);
        }

        static double CNDist(double x)
        {
            double c = 0.0;
            double[]  b = new double[5] { 1.330274429, -1.821255978, 1.781477937, -0.356563782, 0.319381530 };

	        double a = 1.0 / (1.0 + 0.2316419 * Math.Abs(x));

	        for (int ii = 0; ii < 5; ii++) 
            {
		        c += b[ii];
		        c *= a;
	        }
	
	        c *= 0.3989422804 * Math.Exp(-x * x / 2.0);

	        if (x >= 0)
		        c = 1.0 - c;

	        return c;

            /*
            //
            // International Investments, Solnik, p. 292
            //
            double b[6] = { 0.0000005383, 0.0000488906, 0.000380036, 0.0032776263, 0.0211410061, 0.04986734 };

	            for (int ii = 0; ii < 6; ii++) {
		            c += b[ii];
		            c *= x;
	            }
	
	            b += 1.0;
	
	            b = 0.5 * Math.Pow((b + 1.0), -16.0);
	
	            return 1.0 - b;
            */
        }

        double CallPremium(double shortRate, double dividendRate, double volatility, double underlyingPrice, double strikePrice, double term, Style style)
        {
	        if (Math.Abs(term - 0.0) < EPSILON)
		        return Math.Max(underlyingPrice - strikePrice, 0.0);

	        if ((Math.Abs(underlyingPrice - 0.0) < EPSILON) || (Math.Abs(volatility - 0.0) < EPSILON))		// domain error	*****
		        return 0.0;

	        // Black model for European style futures options (no dividends)
	        double e = Math.Exp(-shortRate * term);
	        double d0 = volatility *  Math.Pow(term, 0.5);
	        double xd1 = (Math.Log(underlyingPrice / strikePrice) + d0 * d0 / 2.0) / d0;
	        double xd2 = xd1 - d0;
	        double p = e * (underlyingPrice * CNDist(xd1) - strikePrice * CNDist(xd2));

	        // approximate value for American options
	        // with minor modifications, these are the steps used in the paper 'Efficient Analytic Approximation of 
	        // American Futures Option Values' Giovanni Barone - Adesi and Robert Whaley WP#15 Institute for Financial
	        // Research, Faculty of Business, University of Alberta, Edmonton, Alberta, Canada T6G 2R6
	        if (style == Style.American) 
            {
		        double a = 2.0 * shortRate / (volatility * volatility);
		        double b0 = shortRate - dividendRate;							// non-zero for dividend paying instruments
		        double b = 2.0 * b0 / (volatility * volatility);
		        double h = 1.0 - e;
	
		        double sx = 1.0;												// critical price / strike price
                double l = ((1.0 - b) + Math.Pow(((b - 1.0) * (b - 1.0) + 4.0 * a / h), 0.5)) / 2.0;
        //		double l = (1.0 + Math.Pow((1.0 + 4.0 * a / h), 0.5)) / 2.0;			// b = 0 for non-dividend paying instruments

		        while (sx < 10.0) 
                {
                    double d1 = (Math.Log(sx) + b0 * term + d0 * d0 / 2.0) / d0;
        //			double d1 = (Math.Log(sx) + d0 * d0 / 2.0) / d0;
			        double d2 = d1 - d0;
			        double nd1 = CNDist(d1);
			        double nd2 = CNDist(d2);
			        double lhs = sx - 1.0;
			        double rhs = e * (sx * nd1 - nd2) + sx * (1.0 - e * nd1) / l;

			        // test for convergence
			        if (Math.Abs(lhs - rhs) > EPSILON) 
                    {
				        lhs = e * nd1 * (1.0 - 1.0 / l) + (1.0 - e * CNDistPrime(d1) / d0) / l;
				        if (lhs > 0.999999)										// slope = 1, no early excercise
					        break;
				        sx = (1.0 - lhs * sx + rhs) / (1.0 - lhs);
				        continue;
			        }
			        // converged - calculate correction for American excercise
			        else 
                    {
				        // at or above critical price
				        if (underlyingPrice > sx * strikePrice)
					        p = underlyingPrice - strikePrice;
				        // below critical price
				        else 
                        {
					        double a2 = (1.0 - e * nd1) * sx * strikePrice / l;
					        double a0 = Math.Log(underlyingPrice / (sx * strikePrice)) * l;
					        if (a0 < -60.0)										// domain error
						        a0 = 0.0;
					        else
						        a0 = Math.Exp(a0);
					        p += a2 * a0;										// call price is European price plus correction for American exercise
				        }
				        break;
			        }
		        }
	        }

	        return p;
        }

        double PutPremium(double shortRate, double dividendRate, double volatility, double underlyingPrice, double strikePrice, double term, Style style)
        {
	        if (Math.Abs(term - 0.0) < EPSILON)
		        return Math.Max(strikePrice - underlyingPrice, 0.0);

	        // Black model for European style futures options (no dividends)
	        double e = Math.Exp(-shortRate * term);
	        double p = CallPremium(shortRate, dividendRate, volatility, underlyingPrice, strikePrice, term, Style.European);	// always use European style
	        p -= e * (underlyingPrice - strikePrice);

            if (style == Style.American) 
            {
		        double a = 2.0 * shortRate / (volatility * volatility);
		        double b0 = shortRate - dividendRate;							// non-zero for dividend paying instruments
		        double b = 2.0 * b0 / (volatility * volatility);
		        double h = 1.0 - e;
	
		        double sx = 1.0;												// critical price / strike price
		        double l = ((1.0 - b) - Math.Pow(((b - 1.0) * (b - 1.0) + 4.0 * a / h), 0.5)) / 2.0;
        //		double l = (1.0 - Math.Pow((1.0 + 4.0 * a / h), 0.5)) / 2.0;			// b = 0 for non-dividend paying instruments

		        while (sx > 0.1) 
                {
			        double d0 = volatility * Math.Pow(term, 0.5);
			        double d1 = (Math.Log(sx) + b0 * term + d0 * d0 / 2.0) / d0;
        //			double d1 = (Math.Log(sx) + d0 * d0 / 2.0) / d0;
			        double d2 = d1 - d0;
			        double nd1 = CNDist(d1);
			        double nd2 = CNDist(d2);
			        double lhs = 1.0 - sx;
			        double rhs = e * (sx * (nd1 - 1.0) + (1.0 - nd2)) - sx * (1.0 - e * (1.0 - nd1)) / l;

			        // test for convergence
			        if (Math.Abs(lhs - rhs) > EPSILON) 
                    {
				        lhs = - e * (1.0 - nd1) * (1.0 - 1.0 / l) - (1.0 + e * CNDistPrime(d1) / d0) / l;
				        if (lhs <  - 0.999999)									// slope = -1, no early excercise
					        break;
				        sx = (1.0 + lhs * sx - rhs) / (1.0 + lhs);
				        continue;
			        }
			        // converged - calculate correction for American excercise
			        else 
                    {
				        // at or below critical price
				        if (underlyingPrice < sx * strikePrice)
					        p = strikePrice - underlyingPrice;
				        // above critical price
				        else 
                        {
					        double a1 = - (1.0 - e * (1.0 - nd1)) * sx * strikePrice / l;
					        double a0 = Math.Log(underlyingPrice / (sx * strikePrice)) * l;
					        if (a0 <  -60.0)									// domain error
						        a0 = 0.0;
					        else
						        a0 = Math.Exp(a0);
					        p += a1 * a0;										// put price is European price plus correction for American exercise
				        }
				        break;
			        }
		        }
	        }

	        return p;
        }

        double CallDelta(double shortRate, double dividendRate, double volatility, double underlyingPrice, double strikePrice, double term, Style style)
        {
	        if ((Math.Abs(term - 0.0) < EPSILON) && (Math.Abs(underlyingPrice - strikePrice) < EPSILON))
		        return 0.0; //*****

	        if ((Math.Abs(underlyingPrice - 0.0) < EPSILON) || (Math.Abs(volatility - 0.0) < EPSILON))		// domain error	*****
		        return 0.0;

	        double e = Math.Exp(-shortRate * term);
	        double d0 = volatility * Math.Pow(term, 0.5);
	        double xd1 = (Math.Log(underlyingPrice / strikePrice) + d0 * d0 / 2.0) / d0;
	        double d = e * CNDist(xd1);

            if (style == Style.American) 
            {
		        double a = 2.0 * shortRate / (volatility * volatility);
		        double b0 = shortRate - dividendRate;							// non-zero for dividend paying instruments
		        double b = 2.0 * b0 / (volatility * volatility);
		        double h = 1.0 - e;
	
		        double sx = 1.0;												// critical price / strike price
		        double l = ((1.0 - b) + Math.Pow(((b - 1.0) * (b - 1.0) + 4.0 * a / h), 0.5)) / 2.0;
        //		double l = (1.0 + Math.Pow((1.0 + 4.0 * a / h), 0.5)) / 2.0;			// b = 0 for non-dividend paying instruments

		        while (sx < 10.0) 
                {
			        double d1 = (Math.Log(sx) + b0 * term + d0 * d0 / 2.0) / d0;
        //			double d1 = (Math.Log(sx) + d0 * d0 / 2.0) / d0;
			        double d2 = d1 - d0;
			        double nd1 = CNDist(d1);
			        double nd2 = CNDist(d2);
			        double lhs = sx - 1.0;
			        double rhs = e * (sx * nd1 - nd2) + sx * (1.0 - e * nd1) / l;

			        // test for convergence
			        if (Math.Abs(lhs - rhs) > EPSILON) 
                    {
				        lhs = e * nd1 * (1.0 - 1.0 / l) + (1.0 - e * CNDistPrime(d1) / d0) / l;
				        if (lhs > 0.999999)										// slope = 1, no early excercise
					        break;
				        sx = (1.0 - lhs * sx + rhs) / (1.0 - lhs);
				        continue;
			        }
			        // converged - calculate correction for American excercise
			        else 
                    {
				        // at or above critical price
				        if (underlyingPrice > sx * strikePrice)
					        d = 1.0;
				        // below critical price
				        else {
					        double a2 = (1.0 - e * nd1) * sx * strikePrice / l;
					        double a0 = Math.Log(underlyingPrice / (sx * strikePrice)) * l;
					        if (a0 < -60.0)										// domain error
						        a0 = 0.0;
					        else
						        a0 = Math.Exp(a0);
					        a2 *= l * a0 / underlyingPrice;
					        d += a2;											// call delta is European delta plus correction for American exercise
				        }
				        break;
			        }
		        }
	        }

	        return d;
        }

        double PutDelta(double shortRate, double dividendRate, double volatility, double underlyingPrice, double strikePrice, double term, Style style)
        {
	        if ((Math.Abs(term - 0.0) < EPSILON) && (Math.Abs(underlyingPrice - strikePrice) < EPSILON))
		        return 0.0; //*****

	        if ((Math.Abs(underlyingPrice - 0.0) < EPSILON) || (Math.Abs(volatility - 0.0) < EPSILON))		// domain error	*****
		        return 0.0;

	        double e = Math.Exp(-shortRate * term);
	        double d0 = volatility * Math.Pow(term, 0.5);
	        double xd1 = (Math.Log(underlyingPrice / strikePrice) + d0 * d0 / 2.0) / d0;
	        double d = e * (CNDist(xd1) - 1.0);

            if (style == Style.American) 
            {
		        double a = 2.0 * shortRate / (volatility * volatility);
		        double b0 = shortRate - dividendRate;							// non-zero for dividend paying instruments
		        double b = 2.0 * b0 / (volatility * volatility);
		        double h = 1.0 - e;
	
		        double sx = 1.0;												// critical price / strike price
		        double l = ((1.0 - b) - Math.Pow(((b - 1.0) * (b - 1.0) + 4.0 * a / h), 0.5)) / 2.0;
        //		double l = (1.0 - Math.Pow((1.0 + 4.0 * a / h), 0.5)) / 2.0;			// b = 0 for non-dividend paying instruments

		        while (sx > 0.1) 
                {
			        double d1 = (Math.Log(sx) + b0 * term + d0 * d0 / 2.0) / d0;
        //			double d1 = (Math.Log(sx) + d0 * d0 / 2.0) / d0;
			        double d2 = d1 - d0;
			        double nd1 = CNDist(d1);
			        double nd2 = CNDist(d2);
			        double lhs = 1.0 - sx;
			        double rhs = e * (sx * (nd1 - 1.0) + (1.0 - nd2)) - sx * (1.0 - e * (1.0 - nd1)) / l;

			        // test for convergence
			        if (Math.Abs(lhs - rhs) > EPSILON) 
                    {
				        lhs = - e * (1.0 - nd1) * (1.0 - 1.0 / l) - (1.0 + e * CNDistPrime(d1) / d0) / l;
				        if (lhs <  - 0.999999)									// slope = -1, no early excercise
					        break;
				        sx = (1.0 + lhs * sx - rhs) / (1.0 + lhs);
				        continue;
			        }
			        // converged - calculate correction for American excercise
			        else 
                    {
				        // at or below critical price
				        if (underlyingPrice < sx * strikePrice)
					        d = - 1.0;
				        // above critical price
				        else {
					        double a1 = - (1.0 - e * (1.0 - nd1)) * sx * strikePrice / l;
					        double a0 = Math.Log(underlyingPrice / (sx * strikePrice)) * l;
					        if (a0 <  - 60.0)									// domain error
						        a0 = 0.0;
					        else
						        a0 = Math.Exp(a0);
					        a1 *= l * a0 / underlyingPrice;
					        d += a1;											// put delta is European delta plus correction for American exercise
				        }
				        break;
			        }
		        }
	        }
	
	        return d;
        }

        double CallGamma(double shortRate, double dividendRate, double volatility, double underlyingPrice, double strikePrice, double term, Style style)
        {
	        if (Math.Abs(term - 0.0) < EPSILON)
		        return 0.0;

	        if ((Math.Abs(underlyingPrice - 0.0) < EPSILON) || (Math.Abs(volatility - 0.0) < EPSILON))		// domain error	*****
		        return 0.0;

	        double e = Math.Exp(-shortRate * term);
	        double d0 = volatility * Math.Pow(term, 0.5);
	        double xd1 = (Math.Log(underlyingPrice / strikePrice) + d0 * d0 / 2.0) / d0;
	        double g = e / (underlyingPrice * d0) * CNDistPrime(xd1);

            if (style == Style.American) 
            {
		        double a = 2.0 * shortRate / (volatility * volatility);
		        double b0 = shortRate - dividendRate;							// non-zero for dividend paying instruments
		        double b = 2.0 * b0 / (volatility * volatility);
		        double h = 1.0 - e;
	
		        double sx = 1.0;												// critical price / strike price
		        double l = ((1.0 - b) + Math.Pow(((b - 1.0) * (b - 1.0) + 4.0 * a / h), 0.5)) / 2.0;
        //		double l = (1.0 + Math.Pow((1.0 + 4.0 * a / h), 0.5)) / 2.0;			// b = 0 for non-dividend paying instruments

		        while (sx < 10.0) 
                {
			        double d1 = (Math.Log(sx) + b0 * term + d0 * d0 / 2.0) / d0;
        //			double d1 = (Math.Log(sx) + d0 * d0 / 2.0) / d0;
			        double d2 = d1 - d0;
			        double nd1 = CNDist(d1);
			        double nd2 = CNDist(d2);
			        double lhs = sx - 1.0;
			        double rhs = e * (sx * nd1 - nd2) + sx * (1.0 - e * nd1) / l;

			        // test for convergence
			        if (Math.Abs(lhs - rhs) > EPSILON) 
                    {
				        lhs = e * nd1 * (1.0 - 1.0 / l) + (1.0 - e * CNDistPrime(d1) / d0) / l;
				        if (lhs > 0.999999)										// slope = 1, no early excercise
					        break;
				        sx = (1.0 - lhs * sx + rhs) / (1.0 - lhs);
				        continue;
			        }
			        // converged - calculate correction for American excercise
			        else 
                    {
				        // at or above critical price
				        if (underlyingPrice > sx * strikePrice)
					        g = 0.0;
				        // below critical price
				        else 
                        {
					        double a2 = (1.0 - e * nd1) * sx * strikePrice / l;
					        double a0 = Math.Log(underlyingPrice / (sx * strikePrice)) * l;
					        if (a0 < -60.0)										// domain error
						        a0 = 0.0;
					        else
						        a0 = Math.Exp(a0);
					        a2 *= l * a0 / underlyingPrice;							// call gamma is European gamma plus correction for American exercise
					        g += a2 * (l - 1.0) / underlyingPrice;
				        }
				        break;
			        }
		        }
	        }

	        return g;
        }

        double PutGamma(double shortRate, double dividendRate, double volatility, double underlyingPrice, double strikePrice, double term, Style style)
        {
	        if (Math.Abs(term - 0.0) < EPSILON)
		        return 0.0;

	        if ((Math.Abs(underlyingPrice - 0.0) < EPSILON) || (Math.Abs(volatility - 0.0) < EPSILON))		// domain error	*****
		        return 0.0;

	        double e = Math.Exp(-shortRate * term);
	        double xd0 = volatility * Math.Pow(term, 0.5);
	        double xd1 = (Math.Log(underlyingPrice / strikePrice) + xd0 * xd0 / 2.0) / xd0;
	        double g = e / (underlyingPrice * xd0) * CNDistPrime(xd1);

            if (style == Style.American) 
            {
		        double a = 2.0 * shortRate / (volatility * volatility);
		        double b0 = shortRate - dividendRate;							// non-zero for dividend paying instruments
		        double b = 2.0 * b0 / (volatility * volatility);
		        double h = 1.0 - e;
	
		        double sx = 1.0;												// critical price / strike price
		        double l = ((1.0 - b) - Math.Pow(((b - 1.0) * (b - 1.0) + 4.0 * a / h), 0.5)) / 2.0;
        //		double l = (1.0 - Math.Pow((1.0 + 4.0 * a / h), 0.5)) / 2.0;			// b = 0 for non-dividend paying instruments

		        while (sx > 0.1) 
                {
			        double d0 = volatility * Math.Pow(term, 0.5);
			        double d1 = (Math.Log(sx) + b0 * term + d0 * d0 / 2.0) / d0;
        //			double d1 = (Math.Log(sx) + d0 * d0 / 2.0) / d0;
			        double d2 = d1 - d0;
			        double nd1 = CNDist(d1);
			        double nd2 = CNDist(d2);
			        double lhs = 1.0 - sx;
			        double rhs = e * (sx * (nd1 - 1.0) + (1.0 - nd2)) - sx * (1.0 - e * (1.0 - nd1)) / l;

			        // test for convergence
			        if (Math.Abs(lhs - rhs) > EPSILON) 
                    {
				        lhs = - e * (1.0 - nd1) * (1.0 - 1.0 / l) - (1.0 + e * CNDistPrime(d1) / d0) / l;
				        if (lhs <  - 0.999999)									// slope = -1, no early excercise
					        break;
				        sx = (1.0 + lhs * sx - rhs) / (1.0 + lhs);
				        continue;
			        }
			        // converged - calculate correction for American excercise
			        else 
                    {
				        // at or below critical price
				        if (underlyingPrice < sx * strikePrice)
					        g = 0.0;
				        // above critical price
				        else 
                        {
					        double a1 = - (1.0 - e * (1.0 - nd1)) * sx * strikePrice / l;
					        double a0 = Math.Log(underlyingPrice / (sx * strikePrice)) * l;
					        if (a0 <  - 60.0)									// domain error
						        a0 = 0.0;
					        else
						        a0 = Math.Exp(a0);
					        a1 *= l * a0 / underlyingPrice;							// put gamma is European gamma plus correction for American exercise
					        g += a1 * (l - 1.0) / underlyingPrice;
				        }
				        break;
			        }
		        }
	        }

	        return g;
        }

        double Vega(double shortRate, double dividendRate, double volatility, double underlyingPrice, double strikePrice, double term)
        {
	        if (Math.Abs(term - 0.0) < EPSILON)
		        return 0.0; //*****

	        if ((Math.Abs(underlyingPrice - 0.0) < EPSILON) || (Math.Abs(volatility - 0.0) < EPSILON))		// domain error	*****
		        return 0.0;

	        double d1 = (Math.Log(underlyingPrice / strikePrice) + term * volatility * volatility / 2.0) / (volatility * Math.Pow(term, 0.5));
	        return (Math.Pow(term, 0.5) * underlyingPrice * Math.Exp(-shortRate * term) * CNDistPrime(d1));
        }

        double CallTheta(double shortRate, double dividendRate, double volatility, double underlyingPrice, double strikePrice, double term)
        {
	        if (Math.Abs(term - 0.0) < EPSILON)
		        return 0.0;

	        if ((Math.Abs(underlyingPrice - 0.0) < EPSILON) || (Math.Abs(volatility - 0.0) < EPSILON))		// domain error	*****
		        return 0.0;

	        double d0 = volatility * Math.Pow(term, 0.5);
	        double d1 = (Math.Log(underlyingPrice / strikePrice) + d0 * d0 / 2.0) / d0;
	        double d2 = d1 - d0;

	        return Math.Exp(-shortRate * term) * (CNDistPrime(d1) * ((underlyingPrice * volatility) / (2.0 * Math.Pow(term, 0.5))) + CNDist(d1) * (underlyingPrice * -shortRate) +	CNDist(d2) * (strikePrice * shortRate));
        }

        double PutTheta(double shortRate, double dividendRate, double volatility, double underlyingPrice, double strikePrice, double term)
        {
	        if (Math.Abs(term - 0.0) < EPSILON)
		        return 0.0;

	        if ((Math.Abs(underlyingPrice - 0.0) < EPSILON) || (Math.Abs(volatility - 0.0) < EPSILON))		// domain error	*****
		        return 0.0;

	        double d0 = volatility * Math.Pow(term, 0.5);
	        double d1 = (Math.Log(underlyingPrice / strikePrice) + d0 * d0 / 2.0) / d0;
	        double d2 = d1 - d0;

	        return Math.Exp(-shortRate * term) * (CNDistPrime(d1) * ((underlyingPrice * volatility) / (2.0 * Math.Pow(term, 0.5))) + (CNDist(d1) - 1.0) * (underlyingPrice * -shortRate) + (CNDist(d2) - 1.0)  * (strikePrice * shortRate));
        }

        double CallImpliedVol(double shortRate, double dividendRate, double price, double underlyingPrice, double strikePrice, double term, Style style)
        {
        double dPrc0, dPrc1, dVol0, dVol1;

	        //test for ridiculous conditions
	        if (((strikePrice + price) - underlyingPrice) < EPSILON)
		        return 0.0;

	        // first two guesses
	        dVol0 = 0.02;
	        dVol1 = 0.25;

	        dPrc0 = CallPremium(shortRate, dividendRate, dVol0, underlyingPrice, strikePrice, term, style);

	        for (int ii = 0; ii < MAX_ITER; ii++) {

		        dPrc1 = CallPremium(shortRate, dividendRate, dVol1, underlyingPrice, strikePrice, term, style);

		        // test for convergence
		        if (Math.Abs(price - dPrc1) < EPSILON)
			        return dVol1;

		        // if this doesn't work go to bisection
		        if (Math.Abs(dPrc1 - dPrc0) < EPSILON) {
			        double dNewVol, dNewPrc;

			        dVol0 = 0.02;
			        dVol1 = 0.25;
		
			        dPrc0 = CallPremium(shortRate, dividendRate, dVol0, underlyingPrice, strikePrice, term, style);
			        dPrc1 = CallPremium(shortRate, dividendRate, dVol1, underlyingPrice, strikePrice, term, style);

			        for (int jj = 0; jj < MAX_ITER; jj++) 
                    {
				        if (price < dPrc0)
					        dNewVol = 0.0001;
				        else if (price > dPrc1)
					        dNewVol = 1.00;
				        else
					        dNewVol = dVol0 + (dVol1 - dVol0) / 2.0;
				        dNewPrc = CallPremium(shortRate, dividendRate, dNewVol, underlyingPrice, strikePrice, term, style);
				        // test for convergence
				        if (Math.Abs(price - dNewPrc) < EPSILON)
					        return dNewVol;
				        if ((price < dNewPrc) && (price > dPrc0)) 
                        {
					        dVol1 = dNewVol;
					        dPrc1 = dNewPrc;
				        }
				        else if ((price > dNewPrc) && (price < dPrc1)) 
                        {
					        dVol0 = dNewVol;
					        dPrc0 = dNewPrc;
				        }
				        else
					        return 0.0;
			        }
			        return 0.0;
		        }

		        // calculate next guess
		        double xdNewVol = dVol0 + (dVol1 - dVol0) * (price - dPrc0) / (dPrc1 - dPrc0);
		        dPrc0 = dPrc1;
		        dVol0 = dVol1;
		        dVol1 = xdNewVol;
	        }

	        return 0.0;
        }

        double PutImpliedVol(double shortRate, double dividendRate, double price, double underlyingPrice, double strikePrice, double term, Style style)
        {
        double dPrc0, dPrc1, dVol0, dVol1;

	        //test for ridiculous conditions
	        if (((underlyingPrice + price) - strikePrice) < EPSILON)
		        return 0.0;

	        // first two guesses
	        dVol0 = 0.02;
	        dVol1 = 0.25;

	        dPrc0 = PutPremium(shortRate, dividendRate, dVol0, underlyingPrice, strikePrice, term, style);

	        for (int ii = 0; ii < MAX_ITER; ii++) 
            {
		        dPrc1 = PutPremium(shortRate, dividendRate, dVol1, underlyingPrice, strikePrice, term, style);

		        // test for convergence
		        if (Math.Abs(price - dPrc1) < EPSILON)
			        return dVol1;

		        // if this doesn't work go to bisection
		        if (Math.Abs(dPrc1 - dPrc0) < EPSILON) 
                {
			        double dNewVol, dNewPrc;

			        dVol0 = 0.02;
			        dVol1 = 0.25;
		
			        dPrc0 = PutPremium(shortRate, dividendRate, dVol0, underlyingPrice, strikePrice, term, style);
			        dPrc1 = PutPremium(shortRate, dividendRate, dVol1, underlyingPrice, strikePrice, term, style);

			        for (int jj = 0; jj < MAX_ITER; jj++) 
                    {
				        if (price < dPrc0)
					        dNewVol = 0.0001;
				        else if (price > dPrc1)
					        dNewVol = 1.00;
				        else
					        dNewVol = dVol0 + (dVol1 - dVol0) / 2.0;
				        dNewPrc = PutPremium(shortRate, dividendRate, dNewVol, underlyingPrice, strikePrice, term, style);
				        // test for convergence
				        if (Math.Abs(price - dNewPrc) < EPSILON)
					        return dNewVol;
		 		        if ((price < dNewPrc) && (price > dPrc0)) 
                        {
					        dVol1 = dNewVol;
					        dPrc1 = dNewPrc;
				        }
				        else if ((price > dNewPrc) && (price < dPrc1)) 
                        {
					        dVol0 = dNewVol;
					        dPrc0 = dNewPrc;
				        }
				        else
					        return 0.0;
			        }
			        return 0.0;
		        }

		        // calculate next guess
		        double xdNewVol = dVol0 + (dVol1 - dVol0) * (price - dPrc0) / (dPrc1 - dPrc0);
		        dPrc0 = dPrc1;
		        dVol0 = dVol1;
		        dVol1 = xdNewVol;
	        }

	        return 0.0;
        }

        double StraddleImpliedVol(double shortRate, double dividendRate, double price, double underlyingPrice, double strikePrice, double term, Style style)
        {
        double dPrc0, dPrc1, dVol0, dVol1;

	        //test for ridiculous conditions
	        if (((underlyingPrice + price) - strikePrice) < EPSILON)
		        return 0.0;
	        if (((strikePrice + price) - underlyingPrice) < EPSILON)
		        return 0.0;

	        // first two guesses
	        dVol0 = 0.02;
	        dVol1 = 0.25;

	        dPrc0 = PutPremium(shortRate, dividendRate, dVol0, underlyingPrice, strikePrice, term, style);
	        dPrc0 += CallPremium(shortRate, dividendRate, dVol0, underlyingPrice, strikePrice, term, style);

	        for (int ii = 0; ii < MAX_ITER; ii++) 
            {

		        dPrc1 = PutPremium(shortRate, dividendRate, dVol1, underlyingPrice, strikePrice, term, style);
		        dPrc1 += CallPremium(shortRate, dividendRate, dVol1, underlyingPrice, strikePrice, term, style);

		        // test for convergence
		        if (Math.Abs(price - dPrc1) < EPSILON)
			        return dVol1;

		        // if this doesn't work go to bisection
		        if (Math.Abs(dPrc1 - dPrc0) < EPSILON) 
                {
			        double dNewVol, dNewPrc;

			        dVol0 = 0.02;
			        dVol1 = 0.25;
		
			        dPrc0 = PutPremium(shortRate, dividendRate, dVol0, underlyingPrice, strikePrice, term, style);
			        dPrc0 += CallPremium(shortRate, dividendRate, dVol0, underlyingPrice, strikePrice, term, style);
			        dPrc1 = PutPremium(shortRate, dividendRate, dVol1, underlyingPrice, strikePrice, term, style);
			        dPrc1 += CallPremium(shortRate, dividendRate, dVol1, underlyingPrice, strikePrice, term, style);

			        for (int jj = 0; jj < MAX_ITER; jj++) {
				        if (price < dPrc0)
					        dNewVol = 0.0001;
				        else if (price > dPrc1)
					        dNewVol = 1.00;
				        else
					        dNewVol = dVol0 + (dVol1 - dVol0) / 2.0;
				        dNewPrc = PutPremium(shortRate, dividendRate, dNewVol, underlyingPrice, strikePrice, term, style);
				        dNewPrc += CallPremium(shortRate, dividendRate, dNewVol, underlyingPrice, strikePrice, term, style);
				        // test for convergence
				        if (Math.Abs(price - dNewPrc) < EPSILON)
					        return dNewVol;
				        if ((price < dNewPrc) && (price > dPrc0)) 
                        {
					        dVol1 = dNewVol;
					        dPrc1 = dNewPrc;
				        }
				        else if ((price > dNewPrc) && (price < dPrc1)) 
                        {
					        dVol0 = dNewVol;
					        dPrc0 = dNewPrc;
				        }
				        else
					        return 0.0;
			        }
			        return 0.0;
		        }

		        // calculate next guess
		        double xdNewVol = dVol0 + (dVol1 - dVol0) * (price - dPrc0) / (dPrc1 - dPrc0);
		        dPrc0 = dPrc1;
		        dVol0 = dVol1;
		        dVol1 = xdNewVol;
	        }

	        return 0.0;
        }

        //---------------------------------------------------------------------------------------------
        // Entry points for Eurodollar futures
        //---------------------------------------------------------------------------------------------

        double EuroCallPremium(double shortRate, double volatility, double underlyingPrice, double strikePrice, double term)
        {
	        if (Math.Abs(term - 0.0) < EPSILON)
		        return Math.Max(underlyingPrice - strikePrice, 0.0);

	        return PutPremium(shortRate, 0.0, volatility, 1.0 - underlyingPrice / 100.0, 1.0 - strikePrice / 100.0, term, Style.American) * 100.0;
        }

        double EuroPutPremium(double shortRate, double volatility, double underlyingPrice, double strikePrice, double term)
        {
	        if (Math.Abs(term - 0.0) < EPSILON)
		        return Math.Max(strikePrice - underlyingPrice, 0.0);

	        return CallPremium(shortRate, 0.0, volatility, 1.0 - underlyingPrice / 100.0, 1.0 - strikePrice / 100.0, term, Style.American) * 100.0;
        }

        double EuroCallDelta(double shortRate, double volatility, double underlyingPrice, double strikePrice, double term)
        {
	        return -PutDelta(shortRate, 0.0, volatility, 1.0 - underlyingPrice / 100.0, 1.0 - strikePrice / 100.0, term, Style.American);
        }

        double EuroPutDelta(double shortRate, double volatility, double underlyingPrice, double strikePrice, double term)
        {
	        return -CallDelta(shortRate, 0.0, volatility, 1.0 - underlyingPrice / 100.0, 1.0 - strikePrice / 100.0, term, Style.American);
        }

        double EuroCallGamma(double shortRate, double volatility, double underlyingPrice, double strikePrice, double term)
        {
	        return PutGamma(shortRate, 0.0, volatility, 1.0 - underlyingPrice / 100.0, 1.0 - strikePrice / 100.0, term, Style.American) / 100.0;
        }

        double EuroPutGamma(double shortRate, double volatility, double underlyingPrice, double strikePrice, double term)
        {
	        return CallGamma(shortRate, 0.0, volatility, 1.0 - underlyingPrice / 100.0, 1.0 - strikePrice / 100.0, term, Style.American) / 100.0;
        }

        double EuroVega(double shortRate, double volatility, double underlyingPrice, double strikePrice, double term)
        {
	        return Vega(shortRate, 0.0, volatility, 1.0 - underlyingPrice / 100.0, 1.0 - strikePrice / 100.0, term);
        }

        double EuroCallTheta(double shortRate, double volatility, double underlyingPrice, double strikePrice, double term)
        {
	        return PutTheta(shortRate, 0.0, volatility, 1.0 - underlyingPrice / 100.0, 1.0 - strikePrice / 100.0, term) * 100.0;
        }

        double EuroPutTheta(double shortRate, double volatility, double underlyingPrice, double strikePrice, double term)
        {
	        return CallTheta(shortRate, 0.0, volatility, 1.0 - underlyingPrice / 100.0, 1.0 - strikePrice / 100.0, term) * 100.0;
        }

        double EuroCallImpliedVol(double shortRate, double price, double underlyingPrice, double strikePrice, double term)
        {
	        return PutImpliedVol(shortRate, 0.0, price / 100.0, 1.0 - underlyingPrice / 100.0, 1.0 - strikePrice / 100.0, term, Style.American);
        }

        double EuroPutImpliedVol(double shortRate, double price, double underlyingPrice, double strikePrice, double term)
        {
	        return CallImpliedVol(shortRate, 0.0, price / 100.0, 1.0 - underlyingPrice / 100.0, 1.0 - strikePrice / 100.0, term, Style.American);
        }

        double EuroStraddleImpliedVol(double shortRate, double price, double underlyingPrice, double strikePrice, double term)
        {
	        return StraddleImpliedVol(shortRate, 0.0, price / 100.0, 1.0 - underlyingPrice / 100.0, 1.0 - strikePrice / 100.0, term, Style.American);
        }
    }
}
