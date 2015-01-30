package com.zkc.helper.printer;

import android.content.Context;
import android.os.Handler;

import com.zkc.helper.printer.bt.BtService;

public class PrinterClassFactory {
	public static PrinterClass create(Context _context,Handler _mhandler,Handler _handler){
    return new BtService(_context,_mhandler, _handler); 
  }

}
